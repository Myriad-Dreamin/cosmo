use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::Path;
use std::ptr;
use std::slice;
use std::str;

use url::Url;

pub const COSMO_RUST_FFI_ABI_VERSION: u32 = 1;
pub const COSMO_URI_SYS_OK: u32 = 0;
pub const COSMO_URI_SYS_ERROR_INVALID_INPUT: u32 = 1;
pub const COSMO_URI_SYS_ERROR_INVALID_UTF8: u32 = 2;
pub const COSMO_URI_SYS_ERROR_PANIC: u32 = 3;
pub const COSMO_URI_SYS_ERROR_PARSE: u32 = 100;
pub const COSMO_URI_SYS_ERROR_JOIN: u32 = 101;
pub const COSMO_URI_SYS_ERROR_FILE_PATH: u32 = 102;
pub const COSMO_URI_SYS_ERROR_FILE_URI: u32 = 103;

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUriSysAbiStatus {
    pub abi_version: u32,
    pub value: i32,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUriSysBytes {
    pub capacity: usize,
    pub len: usize,
    pub ptr: *mut u8,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUriSysUriResult {
    pub error: *mut CosmoUriSysError,
    pub uri: *mut CosmoUriSysUri,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUriSysBytesResult {
    pub bytes: CosmoUriSysBytes,
    pub error: *mut CosmoUriSysError,
}

pub struct CosmoUriSysUri {
    url: Url,
}

pub struct CosmoUriSysError {
    kind: u32,
    message: String,
}

#[unsafe(no_mangle)]
pub extern "C" fn cosmo_uri_sys_abi_status() -> CosmoUriSysAbiStatus {
    CosmoUriSysAbiStatus {
        abi_version: COSMO_RUST_FFI_ABI_VERSION,
        value: 1,
    }
}

/// Parses a URI from a caller-owned UTF-8 byte buffer.
///
/// # Safety
///
/// `uri_ptr` must be null only when `uri_len` is 0. Otherwise it must point to
/// `uri_len` readable bytes for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_parse(
    uri_ptr: *const u8,
    uri_len: usize,
) -> CosmoUriSysUriResult {
    catch_uri_result(|| {
        let text = ffi_str(uri_ptr, uri_len, "uri")?;
        parse_url(text)
    })
}

/// Resolves a URI reference against a base URI handle.
///
/// # Safety
///
/// `base` must be null or a live URI handle returned by this crate. The
/// reference buffer follows the same pointer rules as `cosmo_uri_sys_parse`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_join(
    base: *const CosmoUriSysUri,
    reference_ptr: *const u8,
    reference_len: usize,
) -> CosmoUriSysUriResult {
    catch_uri_result(|| {
        let handle = uri_handle(base)?;
        let reference = ffi_str(reference_ptr, reference_len, "reference")?;
        handle
            .url
            .join(reference)
            .map(|url| CosmoUriSysUri { url })
            .map_err(|error| error_value(COSMO_URI_SYS_ERROR_JOIN, error.to_string()))
    })
}

/// Converts a local file path byte buffer into a file URI handle.
///
/// # Safety
///
/// `path_ptr` must be null only when `path_len` is 0. Otherwise it must point to
/// `path_len` readable bytes for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_from_file_path(
    path_ptr: *const u8,
    path_len: usize,
) -> CosmoUriSysUriResult {
    catch_uri_result(|| {
        let text = ffi_str(path_ptr, path_len, "file path")?;
        Url::from_file_path(Path::new(text))
            .map(|url| CosmoUriSysUri { url })
            .map_err(|()| {
                error_value(
                    COSMO_URI_SYS_ERROR_FILE_PATH,
                    "file path cannot be represented as a file URI",
                )
            })
    })
}

/// Converts a URI handle into a local file path byte buffer.
///
/// # Safety
///
/// `uri` must be null or a live URI handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_to_file_path(
    uri: *const CosmoUriSysUri,
) -> CosmoUriSysBytesResult {
    catch_bytes_result(|| {
        let handle = uri_handle(uri)?;
        let path = handle.url.to_file_path().map_err(|()| {
            error_value(
                COSMO_URI_SYS_ERROR_FILE_URI,
                "URI cannot be represented as a local file path",
            )
        })?;
        Ok(bytes_from_vec(
            path.to_string_lossy().into_owned().into_bytes(),
        ))
    })
}

/// Formats a URI handle into a byte buffer.
///
/// # Safety
///
/// `uri` must be null or a live URI handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_display(
    uri: *const CosmoUriSysUri,
) -> CosmoUriSysBytesResult {
    catch_uri_bytes(uri, |url| url.to_string())
}

/// Returns the scheme component of a URI handle.
///
/// # Safety
///
/// `uri` must be null or a live URI handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_scheme(
    uri: *const CosmoUriSysUri,
) -> CosmoUriSysBytesResult {
    catch_uri_bytes(uri, |url| url.scheme().to_string())
}

/// Returns the authority component of a URI handle.
///
/// # Safety
///
/// `uri` must be null or a live URI handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_authority(
    uri: *const CosmoUriSysUri,
) -> CosmoUriSysBytesResult {
    catch_uri_bytes(uri, uri_authority)
}

/// Returns the path component of a URI handle.
///
/// # Safety
///
/// `uri` must be null or a live URI handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_path(uri: *const CosmoUriSysUri) -> CosmoUriSysBytesResult {
    catch_uri_bytes(uri, |url| url.path().to_string())
}

/// Returns the query component of a URI handle.
///
/// # Safety
///
/// `uri` must be null or a live URI handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_query(uri: *const CosmoUriSysUri) -> CosmoUriSysBytesResult {
    catch_uri_bytes(uri, |url| url.query().unwrap_or("").to_string())
}

/// Releases a URI handle allocated by this crate.
///
/// # Safety
///
/// `uri` must be null or a URI handle returned by this crate that has not
/// already been released. The pointer must not be used after this call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_uri_release(uri: *mut CosmoUriSysUri) {
    let _ = catch_unwind(AssertUnwindSafe(|| {
        if !uri.is_null() {
            drop(unsafe { Box::from_raw(uri) });
        }
    }));
}

#[unsafe(no_mangle)]
pub extern "C" fn cosmo_uri_sys_error_ptr_is_some(error: *const CosmoUriSysError) -> u8 {
    u8::from(!error.is_null())
}

/// Returns the stable error kind for an error handle.
///
/// # Safety
///
/// `error` must be null or a live error handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_error_kind(error: *const CosmoUriSysError) -> u32 {
    catch_scalar(|| {
        let error = error_handle(error)?;
        Ok(error.kind)
    })
}

/// Copies the error message into a byte buffer.
///
/// # Safety
///
/// `error` must be null or a live error handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_error_message(
    error: *const CosmoUriSysError,
) -> CosmoUriSysBytes {
    catch_bytes(|| {
        let error = error_handle(error)?;
        Ok(bytes_from_vec(error.message.as_bytes().to_vec()))
    })
}

/// Releases an error handle allocated by this crate.
///
/// # Safety
///
/// `error` must be null or an error handle returned by this crate that has not
/// already been released. The pointer must not be used after this call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_error_release(error: *mut CosmoUriSysError) {
    let _ = catch_unwind(AssertUnwindSafe(|| {
        if !error.is_null() {
            drop(unsafe { Box::from_raw(error) });
        }
    }));
}

/// Releases a byte buffer allocated by this crate.
///
/// # Safety
///
/// `bytes` must be an empty/null buffer or a buffer returned by this crate that
/// has not already been released.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_uri_sys_bytes_release(bytes: CosmoUriSysBytes) {
    let _ = catch_unwind(AssertUnwindSafe(|| {
        if !bytes.ptr.is_null() {
            drop(unsafe { Vec::from_raw_parts(bytes.ptr, bytes.len, bytes.capacity) });
        }
    }));
}

impl CosmoUriSysBytes {
    fn empty() -> Self {
        Self {
            capacity: 0,
            len: 0,
            ptr: ptr::null_mut(),
        }
    }
}

fn parse_url(text: &str) -> Result<CosmoUriSysUri, CosmoUriSysError> {
    Url::parse(text)
        .map(|url| CosmoUriSysUri { url })
        .map_err(|error| error_value(COSMO_URI_SYS_ERROR_PARSE, error.to_string()))
}

fn uri_authority(url: &Url) -> String {
    if !url.has_authority() {
        return String::new();
    }

    let mut authority = String::new();
    if !url.username().is_empty() {
        authority.push_str(url.username());
        if let Some(password) = url.password() {
            authority.push(':');
            authority.push_str(password);
        }
        authority.push('@');
    }
    if let Some(host) = url.host_str() {
        authority.push_str(host);
    }
    if let Some(port) = url.port() {
        authority.push(':');
        authority.push_str(&port.to_string());
    }
    authority
}

fn ffi_bytes<'a>(ptr: *const u8, len: usize, label: &str) -> Result<&'a [u8], CosmoUriSysError> {
    if ptr.is_null() && len == 0 {
        return Ok(&[]);
    }
    if ptr.is_null() {
        return Err(error_value(
            COSMO_URI_SYS_ERROR_INVALID_INPUT,
            format!("{label} pointer is null"),
        ));
    }

    Ok(unsafe { slice::from_raw_parts(ptr, len) })
}

fn ffi_str<'a>(ptr: *const u8, len: usize, label: &str) -> Result<&'a str, CosmoUriSysError> {
    let bytes = ffi_bytes(ptr, len, label)?;

    str::from_utf8(bytes).map_err(|error| {
        error_value(
            COSMO_URI_SYS_ERROR_INVALID_UTF8,
            format!("{label} is not valid UTF-8: {error}"),
        )
    })
}

fn uri_handle<'a>(ptr: *const CosmoUriSysUri) -> Result<&'a CosmoUriSysUri, CosmoUriSysError> {
    unsafe { ptr.as_ref() }
        .ok_or_else(|| error_value(COSMO_URI_SYS_ERROR_INVALID_INPUT, "URI handle is null"))
}

fn error_handle<'a>(
    ptr: *const CosmoUriSysError,
) -> Result<&'a CosmoUriSysError, CosmoUriSysError> {
    unsafe { ptr.as_ref() }
        .ok_or_else(|| error_value(COSMO_URI_SYS_ERROR_INVALID_INPUT, "error handle is null"))
}

fn bytes_from_vec(mut bytes: Vec<u8>) -> CosmoUriSysBytes {
    let result = CosmoUriSysBytes {
        capacity: bytes.capacity(),
        len: bytes.len(),
        ptr: bytes.as_mut_ptr(),
    };
    std::mem::forget(bytes);
    result
}

fn error_value(kind: u32, message: impl Into<String>) -> CosmoUriSysError {
    CosmoUriSysError {
        kind,
        message: message.into(),
    }
}

fn error_ptr(error: CosmoUriSysError) -> *mut CosmoUriSysError {
    Box::into_raw(Box::new(error))
}

fn panic_error() -> CosmoUriSysError {
    error_value(
        COSMO_URI_SYS_ERROR_PANIC,
        "panic crossed uri-sys ABI boundary",
    )
}

fn catch_uri_result(
    function: impl FnOnce() -> Result<CosmoUriSysUri, CosmoUriSysError>,
) -> CosmoUriSysUriResult {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(uri)) => CosmoUriSysUriResult {
            error: ptr::null_mut(),
            uri: Box::into_raw(Box::new(uri)),
        },
        Ok(Err(error)) => CosmoUriSysUriResult {
            error: error_ptr(error),
            uri: ptr::null_mut(),
        },
        Err(_) => CosmoUriSysUriResult {
            error: error_ptr(panic_error()),
            uri: ptr::null_mut(),
        },
    }
}

fn catch_bytes_result(
    function: impl FnOnce() -> Result<CosmoUriSysBytes, CosmoUriSysError>,
) -> CosmoUriSysBytesResult {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(bytes)) => CosmoUriSysBytesResult {
            bytes,
            error: ptr::null_mut(),
        },
        Ok(Err(error)) => CosmoUriSysBytesResult {
            bytes: CosmoUriSysBytes::empty(),
            error: error_ptr(error),
        },
        Err(_) => CosmoUriSysBytesResult {
            bytes: CosmoUriSysBytes::empty(),
            error: error_ptr(panic_error()),
        },
    }
}

fn catch_bytes(
    function: impl FnOnce() -> Result<CosmoUriSysBytes, CosmoUriSysError>,
) -> CosmoUriSysBytes {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(bytes)) => bytes,
        Ok(Err(error)) => bytes_from_vec(error.message.into_bytes()),
        Err(_) => bytes_from_vec(panic_error().message.into_bytes()),
    }
}

fn catch_scalar(function: impl FnOnce() -> Result<u32, CosmoUriSysError>) -> u32 {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(value)) => value,
        Ok(Err(error)) => error.kind,
        Err(_) => COSMO_URI_SYS_ERROR_PANIC,
    }
}

fn catch_uri_bytes(
    uri: *const CosmoUriSysUri,
    function: impl FnOnce(&Url) -> String,
) -> CosmoUriSysBytesResult {
    catch_bytes_result(|| {
        let handle = uri_handle(uri)?;
        Ok(bytes_from_vec(function(&handle.url).into_bytes()))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    unsafe fn input(text: &str) -> (*const u8, usize) {
        (text.as_ptr(), text.len())
    }

    unsafe fn result_text(bytes: CosmoUriSysBytes) -> String {
        let slice = unsafe { slice::from_raw_parts(bytes.ptr, bytes.len) };
        let text = str::from_utf8(slice).unwrap().to_string();
        unsafe {
            cosmo_uri_sys_bytes_release(bytes);
        }
        text
    }

    unsafe fn uri_text(uri: *const CosmoUriSysUri) -> String {
        let result = unsafe { cosmo_uri_sys_display(uri) };
        assert!(result.error.is_null());
        unsafe { result_text(result.bytes) }
    }

    #[test]
    fn parses_and_normalizes_uri_text() {
        unsafe {
            let (ptr, len) = input("HTTPS://Example.COM:443/a/../b?q=1");
            let result = cosmo_uri_sys_parse(ptr, len);

            assert!(result.error.is_null());
            assert_eq!(uri_text(result.uri), "https://example.com/b?q=1");

            cosmo_uri_sys_uri_release(result.uri);
        }
    }

    #[test]
    fn exposes_components_and_joins_references() {
        unsafe {
            let (ptr, len) = input("https://user:pw@example.com/base/index.cos?x=1");
            let base = cosmo_uri_sys_parse(ptr, len);
            assert!(base.error.is_null());

            let scheme = cosmo_uri_sys_scheme(base.uri);
            let authority = cosmo_uri_sys_authority(base.uri);
            let path = cosmo_uri_sys_path(base.uri);
            let query = cosmo_uri_sys_query(base.uri);
            assert_eq!(result_text(scheme.bytes), "https");
            assert_eq!(result_text(authority.bytes), "user:pw@example.com");
            assert_eq!(result_text(path.bytes), "/base/index.cos");
            assert_eq!(result_text(query.bytes), "x=1");

            let (ref_ptr, ref_len) = input("../other.cos");
            let joined = cosmo_uri_sys_join(base.uri, ref_ptr, ref_len);
            assert!(joined.error.is_null());
            assert_eq!(
                uri_text(joined.uri),
                "https://user:pw@example.com/other.cos"
            );

            cosmo_uri_sys_uri_release(joined.uri);
            cosmo_uri_sys_uri_release(base.uri);
        }
    }

    #[test]
    fn round_trips_absolute_file_paths() {
        unsafe {
            let (ptr, len) = input("/tmp/cosmo space/main.cos");
            let uri = cosmo_uri_sys_from_file_path(ptr, len);
            assert!(uri.error.is_null());
            assert_eq!(uri_text(uri.uri), "file:///tmp/cosmo%20space/main.cos");

            let path = cosmo_uri_sys_to_file_path(uri.uri);
            assert!(path.error.is_null());
            assert_eq!(result_text(path.bytes), "/tmp/cosmo space/main.cos");

            cosmo_uri_sys_uri_release(uri.uri);
        }
    }

    #[test]
    fn reports_parse_errors_without_panicking() {
        unsafe {
            let (ptr, len) = input("not a uri");
            let result = cosmo_uri_sys_parse(ptr, len);

            assert!(result.uri.is_null());
            assert!(!result.error.is_null());
            assert_eq!(
                cosmo_uri_sys_error_kind(result.error),
                COSMO_URI_SYS_ERROR_PARSE
            );

            cosmo_uri_sys_error_release(result.error);
        }
    }
}

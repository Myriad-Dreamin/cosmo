use std::io::Read;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::ptr;
use std::slice;
use std::str;
use std::time::Duration;

use ureq::OrAnyStatus;

pub const COSMO_RUST_FFI_ABI_VERSION: u32 = 1;
pub const COSMO_UREQ_SYS_OK: u32 = 0;
pub const COSMO_UREQ_SYS_ERROR_INVALID_INPUT: u32 = 1;
pub const COSMO_UREQ_SYS_ERROR_INVALID_UTF8: u32 = 2;
pub const COSMO_UREQ_SYS_ERROR_CONSUMED: u32 = 3;
pub const COSMO_UREQ_SYS_ERROR_PANIC: u32 = 4;
pub const COSMO_UREQ_SYS_ERROR_BODY_READ: u32 = 5;
pub const COSMO_UREQ_SYS_ERROR_BODY_TOO_LARGE: u32 = 6;
pub const COSMO_UREQ_SYS_ERROR_INVALID_URL: u32 = 100;
pub const COSMO_UREQ_SYS_ERROR_UNKNOWN_SCHEME: u32 = 101;
pub const COSMO_UREQ_SYS_ERROR_DNS: u32 = 102;
pub const COSMO_UREQ_SYS_ERROR_INSECURE_REQUEST_HTTPS_ONLY: u32 = 103;
pub const COSMO_UREQ_SYS_ERROR_CONNECTION_FAILED: u32 = 104;
pub const COSMO_UREQ_SYS_ERROR_TOO_MANY_REDIRECTS: u32 = 105;
pub const COSMO_UREQ_SYS_ERROR_BAD_STATUS: u32 = 106;
pub const COSMO_UREQ_SYS_ERROR_BAD_HEADER: u32 = 107;
pub const COSMO_UREQ_SYS_ERROR_IO: u32 = 108;
pub const COSMO_UREQ_SYS_ERROR_INVALID_PROXY_URL: u32 = 109;
pub const COSMO_UREQ_SYS_ERROR_PROXY_CONNECT: u32 = 110;
pub const COSMO_UREQ_SYS_ERROR_PROXY_UNAUTHORIZED: u32 = 111;
pub const COSMO_UREQ_SYS_ERROR_HTTP_STATUS: u32 = 112;

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUreqSysAbiStatus {
    pub abi_version: u32,
    pub value: i32,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUreqSysBytes {
    pub capacity: usize,
    pub len: usize,
    pub ptr: *mut u8,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUreqSysRequestResult {
    pub error: *mut CosmoUreqSysError,
    pub request: *mut CosmoUreqSysRequest,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUreqSysResponseResult {
    pub error: *mut CosmoUreqSysError,
    pub response: *mut CosmoUreqSysResponse,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUreqSysBytesResult {
    pub bytes: CosmoUreqSysBytes,
    pub error: *mut CosmoUreqSysError,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoUreqSysHeaderResult {
    pub error: *mut CosmoUreqSysError,
    pub found: u8,
    pub value: CosmoUreqSysBytes,
}

pub struct CosmoUreqSysRequest {
    request: Option<ureq::Request>,
}

pub struct CosmoUreqSysResponse {
    response: Option<ureq::Response>,
    status: u16,
    status_text: String,
}

pub struct CosmoUreqSysError {
    kind: u32,
    status: u16,
    message: String,
}

#[unsafe(no_mangle)]
pub extern "C" fn cosmo_ureq_sys_abi_status() -> CosmoUreqSysAbiStatus {
    CosmoUreqSysAbiStatus {
        abi_version: COSMO_RUST_FFI_ABI_VERSION,
        value: 1,
    }
}

/// Creates a request handle from caller-owned UTF-8 byte buffers.
///
/// # Safety
///
/// Pointer arguments must be null only when their matching length is 0.
/// Otherwise they must point to readable bytes for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_request_new(
    method_ptr: *const u8,
    method_len: usize,
    url_ptr: *const u8,
    url_len: usize,
) -> CosmoUreqSysRequestResult {
    catch_request_result(|| {
        let method = ffi_str(method_ptr, method_len, "method")?;
        let url = ffi_str(url_ptr, url_len, "url")?;
        let request = ureq::request(method, url);

        Ok(CosmoUreqSysRequest {
            request: Some(request),
        })
    })
}

/// Sets a header on a request handle.
///
/// # Safety
///
/// `request` must be null or a live request handle returned by this crate. Name
/// and value buffers follow the same pointer rules as
/// `cosmo_ureq_sys_request_new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_request_set_header(
    request: *mut CosmoUreqSysRequest,
    name_ptr: *const u8,
    name_len: usize,
    value_ptr: *const u8,
    value_len: usize,
) -> *mut CosmoUreqSysError {
    catch_error(|| {
        let name = ffi_str(name_ptr, name_len, "header name")?;
        let value = ffi_str(value_ptr, value_len, "header value")?;
        let handle = request_handle_mut(request)?;
        let current = take_request(handle)?;

        handle.request = Some(current.set(name, value));
        Ok(())
    })
}

/// Sets the timeout on a request handle.
///
/// # Safety
///
/// `request` must be null or a live request handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_request_set_timeout_ms(
    request: *mut CosmoUreqSysRequest,
    timeout_ms: u64,
) -> *mut CosmoUreqSysError {
    catch_error(|| {
        let handle = request_handle_mut(request)?;
        let current = take_request(handle)?;

        handle.request = Some(current.timeout(Duration::from_millis(timeout_ms)));
        Ok(())
    })
}

/// Executes a request without a body.
///
/// # Safety
///
/// `request` must be null or a live request handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_request_call(
    request: *mut CosmoUreqSysRequest,
) -> CosmoUreqSysResponseResult {
    catch_response_result(|| {
        let handle = request_handle_mut(request)?;
        let current = take_request(handle)?;

        execute_request(current.call())
    })
}

/// Executes a request with a UTF-8 text body.
///
/// # Safety
///
/// `request` must be null or a live request handle returned by this crate. The
/// body buffer follows the same pointer rules as `cosmo_ureq_sys_request_new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_request_send_text(
    request: *mut CosmoUreqSysRequest,
    body_ptr: *const u8,
    body_len: usize,
) -> CosmoUreqSysResponseResult {
    catch_response_result(|| {
        let body = ffi_str(body_ptr, body_len, "text body")?;
        let handle = request_handle_mut(request)?;
        let current = take_request(handle)?;

        execute_request(current.send_string(body))
    })
}

/// Executes a request with a byte body.
///
/// # Safety
///
/// `request` must be null or a live request handle returned by this crate. The
/// body buffer must be null only when `body_len` is 0; otherwise it must point to
/// `body_len` readable bytes for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_request_send_bytes(
    request: *mut CosmoUreqSysRequest,
    body_ptr: *const u8,
    body_len: usize,
) -> CosmoUreqSysResponseResult {
    catch_response_result(|| {
        let body = ffi_bytes(body_ptr, body_len, "byte body")?;
        let handle = request_handle_mut(request)?;
        let current = take_request(handle)?;

        execute_request(current.send_bytes(body))
    })
}

/// Releases a request handle allocated by this crate.
///
/// # Safety
///
/// `request` must be null or a request handle returned by this crate that has
/// not already been released. The pointer must not be used after this call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_request_release(request: *mut CosmoUreqSysRequest) {
    let _ = catch_unwind(AssertUnwindSafe(|| {
        if !request.is_null() {
            drop(unsafe { Box::from_raw(request) });
        }
    }));
}

/// Returns the status code from a response handle.
///
/// # Safety
///
/// `response` must be null or a live response handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_response_status(
    response: *const CosmoUreqSysResponse,
) -> u16 {
    catch_scalar(|| {
        let handle = response_handle(response)?;
        Ok(handle.status)
    })
}

/// Returns the status text from a response handle.
///
/// # Safety
///
/// `response` must be null or a live response handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_response_status_text(
    response: *const CosmoUreqSysResponse,
) -> CosmoUreqSysBytesResult {
    catch_bytes_result(|| {
        let handle = response_handle(response)?;
        Ok(bytes_from_vec(handle.status_text.as_bytes().to_vec()))
    })
}

/// Looks up a response header by name.
///
/// # Safety
///
/// `response` must be null or a live response handle returned by this crate.
/// `name_ptr` must be null only when `name_len` is 0; otherwise it must point to
/// `name_len` readable bytes for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_response_header(
    response: *const CosmoUreqSysResponse,
    name_ptr: *const u8,
    name_len: usize,
) -> CosmoUreqSysHeaderResult {
    catch_header_result(|| {
        let name = ffi_str(name_ptr, name_len, "header name")?;
        let handle = response_handle(response)?;
        let response = live_response(handle)?;

        match response.header(name) {
            Some(value) => Ok((true, bytes_from_vec(value.as_bytes().to_vec()))),
            None => Ok((false, CosmoUreqSysBytes::empty())),
        }
    })
}

/// Consumes a response handle body as UTF-8 text.
///
/// # Safety
///
/// `response` must be null or a live response handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_response_body_text(
    response: *mut CosmoUreqSysResponse,
) -> CosmoUreqSysBytesResult {
    catch_bytes_result(|| {
        let handle = response_handle_mut(response)?;
        let response = take_response(handle)?;

        response
            .into_string()
            .map(|text| bytes_from_vec(text.into_bytes()))
            .map_err(|error| error_value(COSMO_UREQ_SYS_ERROR_BODY_READ, 0, error.to_string()))
    })
}

/// Consumes a response handle body as bytes up to `max_bytes`.
///
/// # Safety
///
/// `response` must be null or a live response handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_response_body_bytes(
    response: *mut CosmoUreqSysResponse,
    max_bytes: usize,
) -> CosmoUreqSysBytesResult {
    catch_bytes_result(|| {
        let handle = response_handle_mut(response)?;
        let response = take_response(handle)?;
        let mut bytes = Vec::new();
        let limit = max_bytes.saturating_add(1) as u64;
        let read = response.into_reader().take(limit).read_to_end(&mut bytes);

        if let Err(error) = read {
            return Err(error_value(
                COSMO_UREQ_SYS_ERROR_BODY_READ,
                0,
                error.to_string(),
            ));
        }
        if bytes.len() > max_bytes {
            return Err(error_value(
                COSMO_UREQ_SYS_ERROR_BODY_TOO_LARGE,
                0,
                format!("response body exceeded {max_bytes} bytes"),
            ));
        }

        Ok(bytes_from_vec(bytes))
    })
}

/// Performs a blocking GET request and returns the response body bytes.
///
/// # Safety
///
/// `url_ptr` must be null only when `url_len` is 0. Otherwise it must point to
/// `url_len` readable bytes for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_get_text(
    url_ptr: *const u8,
    url_len: usize,
) -> CosmoUreqSysBytesResult {
    catch_bytes_result(|| {
        let url = ffi_str(url_ptr, url_len, "url")?;
        let response = ureq::get(url)
            .timeout(Duration::from_secs(30))
            .call()
            .or_any_status()
            .map_err(error_from_transport)?;
        let mut reader = response.into_reader();
        let mut bytes = Vec::new();
        std::io::Read::read_to_end(&mut reader, &mut bytes)
            .map_err(|error| error_value(COSMO_UREQ_SYS_ERROR_IO, 0, error.to_string()))?;
        Ok(bytes_from_vec(bytes))
    })
}

/// Releases a response handle allocated by this crate.
///
/// # Safety
///
/// `response` must be null or a response handle returned by this crate that has
/// not already been released. The pointer must not be used after this call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_response_release(response: *mut CosmoUreqSysResponse) {
    let _ = catch_unwind(AssertUnwindSafe(|| {
        if !response.is_null() {
            drop(unsafe { Box::from_raw(response) });
        }
    }));
}

#[unsafe(no_mangle)]
pub extern "C" fn cosmo_ureq_sys_error_ptr_is_some(error: *const CosmoUreqSysError) -> u8 {
    u8::from(!error.is_null())
}

/// Returns the stable error kind for an error handle.
///
/// # Safety
///
/// `error` must be null or a live error handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_error_kind(error: *const CosmoUreqSysError) -> u32 {
    catch_scalar(|| {
        let error = error_handle(error)?;
        Ok(error.kind)
    })
}

/// Returns the HTTP status associated with an error handle.
///
/// # Safety
///
/// `error` must be null or a live error handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_error_status(error: *const CosmoUreqSysError) -> u16 {
    catch_scalar(|| {
        let error = error_handle(error)?;
        Ok(error.status)
    })
}

/// Copies the error message into a byte buffer.
///
/// # Safety
///
/// `error` must be null or a live error handle returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cosmo_ureq_sys_error_message(
    error: *const CosmoUreqSysError,
) -> CosmoUreqSysBytes {
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
pub unsafe extern "C" fn cosmo_ureq_sys_error_release(error: *mut CosmoUreqSysError) {
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
pub unsafe extern "C" fn cosmo_ureq_sys_bytes_release(bytes: CosmoUreqSysBytes) {
    let _ = catch_unwind(AssertUnwindSafe(|| {
        if !bytes.ptr.is_null() {
            drop(unsafe { Vec::from_raw_parts(bytes.ptr, bytes.len, bytes.capacity) });
        }
    }));
}

impl CosmoUreqSysBytes {
    fn empty() -> Self {
        Self {
            capacity: 0,
            len: 0,
            ptr: ptr::null_mut(),
        }
    }
}

fn execute_request(
    result: Result<ureq::Response, ureq::Error>,
) -> Result<CosmoUreqSysResponse, CosmoUreqSysError> {
    let response = result.or_any_status().map_err(error_from_transport)?;
    let status = response.status();
    let status_text = response.status_text().to_string();

    Ok(CosmoUreqSysResponse {
        response: Some(response),
        status,
        status_text,
    })
}

fn ffi_bytes<'a>(ptr: *const u8, len: usize, label: &str) -> Result<&'a [u8], CosmoUreqSysError> {
    if ptr.is_null() && len == 0 {
        return Ok(&[]);
    }
    if ptr.is_null() {
        return Err(error_value(
            COSMO_UREQ_SYS_ERROR_INVALID_INPUT,
            0,
            format!("{label} pointer is null"),
        ));
    }

    Ok(unsafe { slice::from_raw_parts(ptr, len) })
}

fn ffi_str<'a>(ptr: *const u8, len: usize, label: &str) -> Result<&'a str, CosmoUreqSysError> {
    let bytes = ffi_bytes(ptr, len, label)?;

    str::from_utf8(bytes).map_err(|error| {
        error_value(
            COSMO_UREQ_SYS_ERROR_INVALID_UTF8,
            0,
            format!("{label} is not valid UTF-8: {error}"),
        )
    })
}

fn request_handle_mut<'a>(
    ptr: *mut CosmoUreqSysRequest,
) -> Result<&'a mut CosmoUreqSysRequest, CosmoUreqSysError> {
    unsafe { ptr.as_mut() }.ok_or_else(|| {
        error_value(
            COSMO_UREQ_SYS_ERROR_INVALID_INPUT,
            0,
            "request handle is null",
        )
    })
}

fn response_handle<'a>(
    ptr: *const CosmoUreqSysResponse,
) -> Result<&'a CosmoUreqSysResponse, CosmoUreqSysError> {
    unsafe { ptr.as_ref() }.ok_or_else(|| {
        error_value(
            COSMO_UREQ_SYS_ERROR_INVALID_INPUT,
            0,
            "response handle is null",
        )
    })
}

fn response_handle_mut<'a>(
    ptr: *mut CosmoUreqSysResponse,
) -> Result<&'a mut CosmoUreqSysResponse, CosmoUreqSysError> {
    unsafe { ptr.as_mut() }.ok_or_else(|| {
        error_value(
            COSMO_UREQ_SYS_ERROR_INVALID_INPUT,
            0,
            "response handle is null",
        )
    })
}

fn error_handle<'a>(
    ptr: *const CosmoUreqSysError,
) -> Result<&'a CosmoUreqSysError, CosmoUreqSysError> {
    unsafe { ptr.as_ref() }.ok_or_else(|| {
        error_value(
            COSMO_UREQ_SYS_ERROR_INVALID_INPUT,
            0,
            "error handle is null",
        )
    })
}

fn take_request(handle: &mut CosmoUreqSysRequest) -> Result<ureq::Request, CosmoUreqSysError> {
    handle.request.take().ok_or_else(|| {
        error_value(
            COSMO_UREQ_SYS_ERROR_CONSUMED,
            0,
            "request has already been consumed",
        )
    })
}

fn live_response(handle: &CosmoUreqSysResponse) -> Result<&ureq::Response, CosmoUreqSysError> {
    handle.response.as_ref().ok_or_else(|| {
        error_value(
            COSMO_UREQ_SYS_ERROR_CONSUMED,
            handle.status,
            "response body has already been consumed",
        )
    })
}

fn take_response(handle: &mut CosmoUreqSysResponse) -> Result<ureq::Response, CosmoUreqSysError> {
    handle.response.take().ok_or_else(|| {
        error_value(
            COSMO_UREQ_SYS_ERROR_CONSUMED,
            handle.status,
            "response body has already been consumed",
        )
    })
}

fn bytes_from_vec(mut bytes: Vec<u8>) -> CosmoUreqSysBytes {
    let result = CosmoUreqSysBytes {
        capacity: bytes.capacity(),
        len: bytes.len(),
        ptr: bytes.as_mut_ptr(),
    };
    std::mem::forget(bytes);
    result
}

fn error_value(kind: u32, status: u16, message: impl Into<String>) -> CosmoUreqSysError {
    CosmoUreqSysError {
        kind,
        status,
        message: message.into(),
    }
}

fn error_ptr(error: CosmoUreqSysError) -> *mut CosmoUreqSysError {
    Box::into_raw(Box::new(error))
}

fn panic_error() -> CosmoUreqSysError {
    error_value(
        COSMO_UREQ_SYS_ERROR_PANIC,
        0,
        "panic crossed ureq-sys ABI boundary",
    )
}

fn error_from_transport(error: ureq::Transport) -> CosmoUreqSysError {
    let kind = match error.kind() {
        ureq::ErrorKind::InvalidUrl => COSMO_UREQ_SYS_ERROR_INVALID_URL,
        ureq::ErrorKind::UnknownScheme => COSMO_UREQ_SYS_ERROR_UNKNOWN_SCHEME,
        ureq::ErrorKind::Dns => COSMO_UREQ_SYS_ERROR_DNS,
        ureq::ErrorKind::InsecureRequestHttpsOnly => {
            COSMO_UREQ_SYS_ERROR_INSECURE_REQUEST_HTTPS_ONLY
        }
        ureq::ErrorKind::ConnectionFailed => COSMO_UREQ_SYS_ERROR_CONNECTION_FAILED,
        ureq::ErrorKind::TooManyRedirects => COSMO_UREQ_SYS_ERROR_TOO_MANY_REDIRECTS,
        ureq::ErrorKind::BadStatus => COSMO_UREQ_SYS_ERROR_BAD_STATUS,
        ureq::ErrorKind::BadHeader => COSMO_UREQ_SYS_ERROR_BAD_HEADER,
        ureq::ErrorKind::Io => COSMO_UREQ_SYS_ERROR_IO,
        ureq::ErrorKind::InvalidProxyUrl => COSMO_UREQ_SYS_ERROR_INVALID_PROXY_URL,
        ureq::ErrorKind::ProxyConnect => COSMO_UREQ_SYS_ERROR_PROXY_CONNECT,
        ureq::ErrorKind::ProxyUnauthorized => COSMO_UREQ_SYS_ERROR_PROXY_UNAUTHORIZED,
        ureq::ErrorKind::HTTP => COSMO_UREQ_SYS_ERROR_HTTP_STATUS,
    };

    error_value(kind, 0, error.to_string())
}

fn catch_error(function: impl FnOnce() -> Result<(), CosmoUreqSysError>) -> *mut CosmoUreqSysError {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(())) => ptr::null_mut(),
        Ok(Err(error)) => error_ptr(error),
        Err(_) => error_ptr(panic_error()),
    }
}

fn catch_request_result(
    function: impl FnOnce() -> Result<CosmoUreqSysRequest, CosmoUreqSysError>,
) -> CosmoUreqSysRequestResult {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(request)) => CosmoUreqSysRequestResult {
            error: ptr::null_mut(),
            request: Box::into_raw(Box::new(request)),
        },
        Ok(Err(error)) => CosmoUreqSysRequestResult {
            error: error_ptr(error),
            request: ptr::null_mut(),
        },
        Err(_) => CosmoUreqSysRequestResult {
            error: error_ptr(panic_error()),
            request: ptr::null_mut(),
        },
    }
}

fn catch_response_result(
    function: impl FnOnce() -> Result<CosmoUreqSysResponse, CosmoUreqSysError>,
) -> CosmoUreqSysResponseResult {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(response)) => CosmoUreqSysResponseResult {
            error: ptr::null_mut(),
            response: Box::into_raw(Box::new(response)),
        },
        Ok(Err(error)) => CosmoUreqSysResponseResult {
            error: error_ptr(error),
            response: ptr::null_mut(),
        },
        Err(_) => CosmoUreqSysResponseResult {
            error: error_ptr(panic_error()),
            response: ptr::null_mut(),
        },
    }
}

fn catch_bytes_result(
    function: impl FnOnce() -> Result<CosmoUreqSysBytes, CosmoUreqSysError>,
) -> CosmoUreqSysBytesResult {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(bytes)) => CosmoUreqSysBytesResult {
            bytes,
            error: ptr::null_mut(),
        },
        Ok(Err(error)) => CosmoUreqSysBytesResult {
            bytes: CosmoUreqSysBytes::empty(),
            error: error_ptr(error),
        },
        Err(_) => CosmoUreqSysBytesResult {
            bytes: CosmoUreqSysBytes::empty(),
            error: error_ptr(panic_error()),
        },
    }
}

fn catch_header_result(
    function: impl FnOnce() -> Result<(bool, CosmoUreqSysBytes), CosmoUreqSysError>,
) -> CosmoUreqSysHeaderResult {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok((found, value))) => CosmoUreqSysHeaderResult {
            error: ptr::null_mut(),
            found: u8::from(found),
            value,
        },
        Ok(Err(error)) => CosmoUreqSysHeaderResult {
            error: error_ptr(error),
            found: 0,
            value: CosmoUreqSysBytes::empty(),
        },
        Err(_) => CosmoUreqSysHeaderResult {
            error: error_ptr(panic_error()),
            found: 0,
            value: CosmoUreqSysBytes::empty(),
        },
    }
}

fn catch_bytes(
    function: impl FnOnce() -> Result<CosmoUreqSysBytes, CosmoUreqSysError>,
) -> CosmoUreqSysBytes {
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(bytes)) => bytes,
        Ok(Err(_)) | Err(_) => CosmoUreqSysBytes::empty(),
    }
}

fn catch_scalar<T>(function: impl FnOnce() -> Result<T, CosmoUreqSysError>) -> T
where
    T: Default,
{
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(value)) => value,
        Ok(Err(_)) | Err(_) => T::default(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::net::TcpListener;
    use std::thread::{self, JoinHandle};

    #[test]
    fn status_reports_shared_abi_version() {
        assert_eq!(
            cosmo_ureq_sys_abi_status(),
            CosmoUreqSysAbiStatus {
                abi_version: COSMO_RUST_FFI_ABI_VERSION,
                value: 1,
            },
        );
    }

    #[test]
    fn blocking_get_reads_status_header_and_text_body_from_local_http() {
        let (url, server) = serve_once(
            b"HTTP/1.1 200 OK\r\nContent-Length: 11\r\nX-Test: local\r\nConnection: close\r\n\r\nhello local"
                .to_vec(),
        );

        unsafe {
            let request = request_new("GET", &url);
            assert_no_error(cosmo_ureq_sys_request_set_header(
                request,
                b"Accept".as_ptr(),
                b"Accept".len(),
                b"text/plain".as_ptr(),
                b"text/plain".len(),
            ));
            assert_no_error(cosmo_ureq_sys_request_set_timeout_ms(request, 5_000));

            let response_result = cosmo_ureq_sys_request_call(request);
            assert_no_error(response_result.error);
            assert_eq!(
                cosmo_ureq_sys_response_status(response_result.response),
                200
            );

            let header = cosmo_ureq_sys_response_header(
                response_result.response,
                b"x-test".as_ptr(),
                b"x-test".len(),
            );
            assert_no_error(header.error);
            assert_eq!(header.found, 1);
            assert_eq!(bytes_to_vec(header.value), b"local");

            let body = cosmo_ureq_sys_response_body_text(response_result.response);
            assert_no_error(body.error);
            assert_eq!(bytes_to_vec(body.bytes), b"hello local");

            cosmo_ureq_sys_response_release(response_result.response);
        }

        let request = server.join().expect("server thread should finish");
        assert!(request.starts_with("GET / HTTP/1.1"), "{request}");
        assert!(request.contains("Accept: text/plain"), "{request}");
    }

    #[test]
    fn post_bytes_reads_bounded_response_bytes_from_local_http() {
        let (url, server) = serve_once(
            b"HTTP/1.1 201 Created\r\nContent-Length: 4\r\nConnection: close\r\n\r\npong".to_vec(),
        );

        unsafe {
            let request = request_new("POST", &url);
            let response_result =
                cosmo_ureq_sys_request_send_bytes(request, b"ping".as_ptr(), b"ping".len());
            assert_no_error(response_result.error);
            assert_eq!(
                cosmo_ureq_sys_response_status(response_result.response),
                201
            );

            let body = cosmo_ureq_sys_response_body_bytes(response_result.response, 16);
            assert_no_error(body.error);
            assert_eq!(bytes_to_vec(body.bytes), b"pong");

            cosmo_ureq_sys_response_release(response_result.response);
        }

        let request = server.join().expect("server thread should finish");
        assert!(request.starts_with("POST / HTTP/1.1"), "{request}");
        assert!(request.ends_with("ping"), "{request}");
    }

    #[test]
    fn status_responses_are_returned_for_inspection() {
        let (url, server) = serve_once(
            b"HTTP/1.1 404 Not Found\r\nContent-Length: 7\r\nConnection: close\r\n\r\nmissing"
                .to_vec(),
        );

        unsafe {
            let request = request_new("GET", &url);
            let response_result = cosmo_ureq_sys_request_call(request);
            assert_no_error(response_result.error);
            assert_eq!(
                cosmo_ureq_sys_response_status(response_result.response),
                404
            );

            let status_text = cosmo_ureq_sys_response_status_text(response_result.response);
            assert_no_error(status_text.error);
            assert_eq!(bytes_to_vec(status_text.bytes), b"Not Found");

            let body = cosmo_ureq_sys_response_body_text(response_result.response);
            assert_no_error(body.error);
            assert_eq!(bytes_to_vec(body.bytes), b"missing");

            cosmo_ureq_sys_response_release(response_result.response);
        }

        server.join().expect("server thread should finish");
    }

    #[test]
    fn invalid_inputs_map_to_stable_error_kinds_without_network_io() {
        unsafe {
            assert_eq!(cosmo_ureq_sys_error_ptr_is_some(ptr::null()), 0);

            let invalid_url = [0xffu8];
            let invalid_utf8 = cosmo_ureq_sys_request_new(
                b"GET".as_ptr(),
                b"GET".len(),
                invalid_url.as_ptr(),
                invalid_url.len(),
            );
            assert_eq!(cosmo_ureq_sys_error_ptr_is_some(invalid_utf8.error), 1);
            assert_error_kind(invalid_utf8.error, COSMO_UREQ_SYS_ERROR_INVALID_UTF8);
            assert!(invalid_utf8.request.is_null());

            let null_body = cosmo_ureq_sys_request_send_bytes(ptr::null_mut(), ptr::null(), 1);
            assert_error_kind(null_body.error, COSMO_UREQ_SYS_ERROR_INVALID_INPUT);
            assert!(null_body.response.is_null());
        }
    }

    #[test]
    fn transport_errors_map_to_stable_error_kinds() {
        unsafe {
            let request = request_new("GET", "not a url");
            let response = cosmo_ureq_sys_request_call(request);
            assert_error_kind(response.error, COSMO_UREQ_SYS_ERROR_INVALID_URL);
            assert!(response.response.is_null());
        }
    }

    fn serve_once(response: Vec<u8>) -> (String, JoinHandle<String>) {
        let listener = TcpListener::bind("127.0.0.1:0").expect("local listener should bind");
        let url = format!(
            "http://{}",
            listener
                .local_addr()
                .expect("listener should have an address")
        );
        let handle = thread::spawn(move || {
            let (mut stream, _) = listener
                .accept()
                .expect("server should accept one connection");
            let request = read_http_request(&mut stream);
            stream
                .write_all(&response)
                .expect("server should write response");
            request
        });

        (url, handle)
    }

    fn read_http_request(stream: &mut impl Read) -> String {
        let mut bytes = Vec::new();
        let header_end = read_until_headers(stream, &mut bytes);
        let content_length = content_length(&bytes[..header_end]).unwrap_or(0);
        let expected_len = header_end + content_length;

        while bytes.len() < expected_len {
            let mut buffer = [0; 512];
            let read = stream
                .read(&mut buffer)
                .expect("server should read request body");
            if read == 0 {
                break;
            }
            bytes.extend_from_slice(&buffer[..read]);
        }

        String::from_utf8_lossy(&bytes).to_string()
    }

    fn read_until_headers(stream: &mut impl Read, bytes: &mut Vec<u8>) -> usize {
        loop {
            if let Some(index) = header_end(bytes) {
                return index;
            }

            let mut buffer = [0; 512];
            let read = stream
                .read(&mut buffer)
                .expect("server should read request headers");
            assert!(
                read > 0,
                "connection closed before request headers finished"
            );
            bytes.extend_from_slice(&buffer[..read]);
        }
    }

    fn header_end(bytes: &[u8]) -> Option<usize> {
        bytes
            .windows(4)
            .position(|window| window == b"\r\n\r\n")
            .map(|index| index + 4)
    }

    fn content_length(headers: &[u8]) -> Option<usize> {
        let text = String::from_utf8_lossy(headers);
        text.lines().find_map(|line| {
            let (name, value) = line.split_once(':')?;
            if !name.eq_ignore_ascii_case("content-length") {
                return None;
            }
            value.trim().parse().ok()
        })
    }

    unsafe fn request_new(method: &str, url: &str) -> *mut CosmoUreqSysRequest {
        let result = unsafe {
            cosmo_ureq_sys_request_new(method.as_ptr(), method.len(), url.as_ptr(), url.len())
        };
        unsafe {
            assert_no_error(result.error);
        }
        assert!(!result.request.is_null());
        result.request
    }

    unsafe fn assert_no_error(error: *mut CosmoUreqSysError) {
        if error.is_null() {
            return;
        }

        let kind = unsafe { cosmo_ureq_sys_error_kind(error) };
        let message = unsafe {
            String::from_utf8_lossy(&bytes_to_vec(cosmo_ureq_sys_error_message(error))).to_string()
        };
        unsafe {
            cosmo_ureq_sys_error_release(error);
        }
        panic!("unexpected ureq-sys error {kind}: {message}");
    }

    unsafe fn assert_error_kind(error: *mut CosmoUreqSysError, expected: u32) {
        assert!(!error.is_null(), "expected an error handle");
        assert_eq!(unsafe { cosmo_ureq_sys_error_kind(error) }, expected);
        unsafe {
            cosmo_ureq_sys_error_release(error);
        }
    }

    unsafe fn bytes_to_vec(bytes: CosmoUreqSysBytes) -> Vec<u8> {
        if bytes.ptr.is_null() {
            return Vec::new();
        }

        let result = unsafe { slice::from_raw_parts(bytes.ptr, bytes.len) }.to_vec();
        unsafe {
            cosmo_ureq_sys_bytes_release(bytes);
        }
        result
    }
}

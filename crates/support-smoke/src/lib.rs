pub const COSMO_RUST_FFI_ABI_VERSION: u32 = 1;

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CosmoSupportSmokeStatus {
    pub abi_version: u32,
    pub value: i32,
}

#[no_mangle]
pub extern "C" fn cosmo_support_smoke_add(lhs: i32, rhs: i32) -> i32 {
    lhs + rhs
}

#[no_mangle]
pub extern "C" fn cosmo_support_smoke_status() -> CosmoSupportSmokeStatus {
    CosmoSupportSmokeStatus {
        abi_version: COSMO_RUST_FFI_ABI_VERSION,
        value: 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_adds_scalars_through_c_abi_shape() {
        assert_eq!(cosmo_support_smoke_add(20, 22), 42);
    }

    #[test]
    fn smoke_status_reports_shared_abi_version() {
        assert_eq!(
            cosmo_support_smoke_status(),
            CosmoSupportSmokeStatus {
                abi_version: COSMO_RUST_FFI_ABI_VERSION,
                value: 1,
            },
        );
    }
}

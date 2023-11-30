use crate::{
    ffi::{mpv_format, mpv_node, Data},
    Node, RustOwnedNode, NodeList,
};

use super::ffi::mpv_free;

use std::{
    ffi::{c_char, c_longlong, c_void, CStr, CString},
    mem::MaybeUninit,
};

/// # Safety
///
/// `from_ptr` should not take ownship of `ptr`.
/// `MPV_FORMAT` should be corresponding to the `Self::Raw` type whose pointer will be passed to mpv.
pub unsafe trait FromMpv {
    const MPV_FORMAT: mpv_format;

    type Raw;

    /// # Safety
    ///
    /// `ptr` should be valid and its underlying type should be corresponding to `Self::MPV_FORMAT`.
    unsafe fn from_ptr(ptr: *const c_void) -> Self;

    /// # Safety
    ///
    /// `inited` should be properly inited.
    unsafe fn from_raw(inited: MaybeUninit<Self::Raw>) -> Self;
}

unsafe impl FromMpv for i64 {
    const MPV_FORMAT: mpv_format = mpv_format::MPV_FORMAT_INT64;

    type Raw = c_longlong;

    unsafe fn from_ptr(ptr: *const c_void) -> Self {
        *(ptr as *const c_longlong) as Self
    }

    unsafe fn from_raw(inited: MaybeUninit<Self::Raw>) -> Self {
        inited.assume_init() as Self
    }
}

unsafe impl FromMpv for String {
    const MPV_FORMAT: mpv_format = mpv_format::MPV_FORMAT_STRING;

    type Raw = *const c_char;

    unsafe fn from_ptr(ptr: *const c_void) -> Self {
        let ptr = ptr as *const *const c_char;
        CStr::from_ptr(*ptr).to_string_lossy().into_owned()
    }

    unsafe fn from_raw(inited: MaybeUninit<Self::Raw>) -> Self {
        let ptr = inited.assume_init();
        let res = <Self as FromMpv>::from_ptr(&ptr as *const *const c_char as _);
        mpv_free(ptr as _);
        res
    }
}

unsafe impl FromMpv for Node {
    const MPV_FORMAT: mpv_format = mpv_format::MPV_FORMAT_NODE;

    type Raw = mpv_node;

    unsafe fn from_ptr(ptr: *const c_void) -> Self {
        Self::from_mpv_node(&*(ptr as *const _))
    }

    unsafe fn from_raw(inited: MaybeUninit<Self::Raw>) -> Self {
        Self::from_mpv_node(&inited.assume_init())
    }
}

/// # Safety
///
/// pointers in returned `mpv_node` should live longer enough.
/// implementations should set `mpv_node.format` correctly
pub unsafe trait ToMpv {
    fn to_node(self) -> RustOwnedNode;
}

unsafe impl ToMpv for String {
    fn to_node(self) -> RustOwnedNode {
        self.as_str().to_node()
    }
}

unsafe impl ToMpv for &str {
    fn to_node(self) -> RustOwnedNode {
        let s = CString::new(self).unwrap();
        RustOwnedNode(mpv_node {
            u: Data { string: s.into_raw() },
            format: mpv_format::MPV_FORMAT_STRING,
        })
    }
}

unsafe impl<T: Into<Node>> ToMpv for Vec<T> {
    fn to_node(self) -> RustOwnedNode {
        let nodes = self.into_iter().map(|v| v.into()).collect();
        Node::Array(NodeList(nodes)).to_node()
    }
}

unsafe impl ToMpv for Node {
    fn to_node(self) -> RustOwnedNode {
        RustOwnedNode(self.to_raw_node())
    }
}
#![allow(non_upper_case_globals)]

use std::ffi::{c_char, c_double, c_int, c_longlong, c_ulonglong, c_void};

use libloading::{Library, Symbol};
use once_cell::sync::Lazy;

type LazySymbol<T> = Lazy<Symbol<'static, T>>;

pub static MPV_LIB: Lazy<Library> = Lazy::new(|| unsafe { Library::new("mpv.exe").unwrap() });

pub static mpv_error_string: LazySymbol<unsafe extern "C" fn(mpv_error) -> *const c_char> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_error_string").unwrap() });

pub static mpv_free: LazySymbol<unsafe extern "C" fn(*mut c_void) -> ()> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_free").unwrap() });

pub static mpv_client_name: LazySymbol<unsafe extern "C" fn(*mut mpv_handle) -> *const c_char> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_client_name").unwrap() });

pub static mpv_client_id: LazySymbol<unsafe extern "C" fn(*mut mpv_handle) -> c_longlong> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_client_id").unwrap() });

pub static mpv_create: LazySymbol<unsafe extern "C" fn() -> *mut mpv_handle> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_create").unwrap() });

pub static mpv_initialize: LazySymbol<unsafe extern "C" fn(*mut mpv_handle) -> mpv_error> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_initialize").unwrap() });

pub static mpv_destroy: LazySymbol<unsafe extern "C" fn(*mut mpv_handle) -> ()> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_destroy").unwrap() });

pub static mpv_create_client: LazySymbol<unsafe extern "C" fn(*mut mpv_handle, *const c_char) -> *mut mpv_handle> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_create_client").unwrap() });

pub static mpv_create_weak_client: LazySymbol<unsafe extern "C" fn(*mut mpv_handle, *const c_char) -> *mut mpv_handle> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_create_weak_client").unwrap() });

pub static mpv_command: LazySymbol<unsafe extern "C" fn(*mut mpv_handle, *const *const c_char) -> mpv_error> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_command").unwrap() });

pub static mpv_command_async: LazySymbol<
    unsafe extern "C" fn(*mut mpv_handle, c_ulonglong, *const *const c_char) -> mpv_error,
> = Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_command_async").unwrap() });

pub static mpv_set_property: LazySymbol<
    unsafe extern "C" fn(*mut mpv_handle, *const c_char, c_int, *const c_void) -> mpv_error,
> = Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_set_property").unwrap() });

pub static mpv_get_property: LazySymbol<
    unsafe extern "C" fn(*mut mpv_handle, *const c_char, c_int, *const c_void) -> mpv_error,
> = Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_get_property").unwrap() });

pub static mpv_observe_property: LazySymbol<
    unsafe extern "C" fn(*mut mpv_handle, c_ulonglong, *const c_char, c_int) -> mpv_error,
> = Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_observe_property").unwrap() });

pub static mpv_unobserve_property: LazySymbol<unsafe extern "C" fn(*mut mpv_handle, c_ulonglong) -> mpv_error> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_unobserve_property").unwrap() });

pub static mpv_event_name: LazySymbol<unsafe extern "C" fn(mpv_event_id) -> *const c_char> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_event_name").unwrap() });

pub static mpv_wait_event: LazySymbol<unsafe extern "C" fn(*mut mpv_handle, c_double) -> *mut mpv_event> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_wait_event").unwrap() });

pub static mpv_hook_add: LazySymbol<
    unsafe extern "C" fn(*mut mpv_handle, c_ulonglong, *const c_char, c_int) -> mpv_error,
> = Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_hook_add").unwrap() });

pub static mpv_hook_continue: LazySymbol<unsafe extern "C" fn(*mut mpv_handle, c_ulonglong) -> mpv_error> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_hook_continue").unwrap() });

pub static mpv_free_node_contents: LazySymbol<unsafe extern "C" fn(*mut mpv_node) -> ()> =
    Lazy::new(|| unsafe { MPV_LIB.get(b"mpv_free_node_contents").unwrap() });

#[repr(C)]
#[allow(non_camel_case_types, unused)]
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum mpv_format {
    /// Invalid. Sometimes used for empty values. This is always defined to 0,
    /// so a normal 0-init of mpv_format (or e.g. mpv_node) is guaranteed to set
    /// this it to MPV_FORMAT_NONE (which makes some things saner as consequence).
    MPV_FORMAT_NONE = 0,
    /// The basic type is `char*`. It returns the raw property string, like
    /// using `${=property}` in input.conf (see input.rst).
    ///
    /// `NULL` isn't an allowed value.
    ///
    /// Warning: although the encoding is usually UTF-8, this is not always the
    ///          case. File tags often store strings in some legacy codepage,
    ///          and even filenames don't necessarily have to be in UTF-8 (at
    ///          least on Linux). If you pass the strings to code that requires
    ///          valid UTF-8, you have to sanitize it in some way.
    ///          On Windows, filenames are always UTF-8, and libmpv converts
    ///          between UTF-8 and UTF-16 when using win32 API functions. See
    ///          the "Encoding of filenames" section for details.
    ///
    /// Example for reading:
    /// ```C
    ///     char *result = NULL;
    ///     if (mpv_get_property(ctx, "property", MPV_FORMAT_STRING, &result) < 0)
    ///         goto error;
    ///     printf("%s\n", result);
    ///     mpv_free(result);
    /// ```
    /// Or just use mpv_get_property_string().
    ///
    /// Example for writing:
    /// ```C
    ///     char *value = "the new value";
    ///     // yep, you pass the address to the variable
    ///     // (needed for symmetry with other types and mpv_get_property)
    ///     mpv_set_property(ctx, "property", MPV_FORMAT_STRING, &value);
    /// ```
    /// Or just use mpv_set_property_string().
    MPV_FORMAT_STRING = 1,
    /// The basic type is `char*`. It returns the OSD property string, like
    /// using `${property}` in input.conf (see input.rst). In many cases, this
    /// is the same as the raw string, but in other cases it's formatted for
    /// display on OSD. It's intended to be human readable. Do not attempt to
    /// parse these strings.
    ///
    /// Only valid when doing read access. The rest works like MPV_FORMAT_STRING.
    MPV_FORMAT_OSD_STRING = 2,
    /// The basic type is `int`. The only allowed values are 0 ("no")
    /// and 1 ("yes").
    ///
    /// Example for reading:
    /// ```C
    ///     int result;
    ///     if (mpv_get_property(ctx, "property", MPV_FORMAT_FLAG, &result) < 0)
    ///         goto error;
    ///     printf("%s\n", result ? "true" : "false");
    /// ```
    /// Example for writing:
    /// ```C
    ///     int flag = 1;
    ///     mpv_set_property(ctx, "property", MPV_FORMAT_FLAG, &flag);
    /// ```
    MPV_FORMAT_FLAG = 3,
    ///
    ///The basic type is `int64_t`.
    ///
    MPV_FORMAT_INT64 = 4,
    ///
    /// The basic type is `double`.
    ///
    MPV_FORMAT_DOUBLE = 5,
    /**
     * The type is mpv_node.
     *
     * For reading, you usually would pass a pointer to a stack-allocated
     * mpv_node value to mpv, and when you're done you call
     * mpv_free_node_contents(&node).
     * You're expected not to write to the data - if you have to, copy it
     * first (which you have to do manually).
     *
     * For writing, you construct your own mpv_node, and pass a pointer to the
     * API. The API will never write to your data (and copy it if needed), so
     * you're free to use any form of allocation or memory management you like.
     *
     * Warning: when reading, always check the mpv_node.format member. For
     *          example, properties might change their type in future versions
     *          of mpv, or sometimes even during runtime.
     *
     * Example for reading:
     *
     *     mpv_node result;
     *     if (mpv_get_property(ctx, "property", MPV_FORMAT_NODE, &result) < 0)
     *         goto error;
     *     printf("format=%d\n", (int)result.format);
     *     mpv_free_node_contents(&result).
     *
     * Example for writing:
     *
     *     mpv_node value;
     *     value.format = MPV_FORMAT_STRING;
     *     value.u.string = "hello";
     *     mpv_set_property(ctx, "property", MPV_FORMAT_NODE, &value);
     */
    MPV_FORMAT_NODE = 6,
    /**
     * Used with mpv_node only. Can usually not be used directly.
     */
    MPV_FORMAT_NODE_ARRAY = 7,
    /**
     * See MPV_FORMAT_NODE_ARRAY.
     */
    MPV_FORMAT_NODE_MAP = 8,
    /**
     * A raw, untyped byte array. Only used only with mpv_node, and only in
     * some very specific situations. (Some commands use it.)
     */
    MPV_FORMAT_BYTE_ARRAY = 9,
}

#[repr(i32)]
#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum mpv_error {
    SUCCESS = 0,
    EVENT_QUEUE_FULL = -1,
    NOMEM = -2,
    UNINITIALIZED = -3,
    INVALID_PARAMETER = -4,
    OPTION_NOT_FOUND = -5,
    OPTION_FORMAT = -6,
    OPTION_ERROR = -7,
    PROPERTY_NOT_FOUND = -8,
    PROPERTY_FORMAT = -9,
    PROPERTY_UNAVAILABLE = -10,
    PROPERTY_ERROR = -11,
    COMMAND = -12,
    LOADING_FAILED = -13,
    AO_INIT_FAILED = -14,
    VO_INIT_FAILED = -15,
    NOTHING_TO_PLAY = -16,
    UNKNOWN_FORMAT = -17,
    UNSUPPORTED = -18,
    NOT_IMPLEMENTED = -19,
    GENERIC = -20,
}

#[repr(i32)]
#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq)]
pub enum mpv_event_id {
    NONE = 0,
    SHUTDOWN = 1,
    LOG_MESSAGE = 2,
    GET_PROPERTY_REPLY = 3,
    SET_PROPERTY_REPLY = 4,
    COMMAND_REPLY = 5,
    START_FILE = 6,
    END_FILE = 7,
    FILE_LOADED = 8,
    CLIENT_MESSAGE = 16,
    VIDEO_RECONFIG = 17,
    AUDIO_RECONFIG = 18,
    SEEK = 20,
    PLAYBACK_RESTART = 21,
    PROPERTY_CHANGE = 22,
    QUEUE_OVERFLOW = 24,
    HOOK = 25,
}

#[repr(i32)]
#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq)]
pub enum mpv_log_level {
    MPV_LOG_LEVEL_NONE = 0,
    MPV_LOG_LEVEL_FATAL = 10,
    MPV_LOG_LEVEL_ERROR = 20,
    MPV_LOG_LEVEL_WARN = 30,
    MPV_LOG_LEVEL_INFO = 40,
    MPV_LOG_LEVEL_V = 50,
    MPV_LOG_LEVEL_DEBUG = 60,
    MPV_LOG_LEVEL_TRACE = 70,
}

#[repr(i32)]
#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq)]
pub enum mpv_end_file_reason {
    MPV_END_FILE_REASON_EOF = 0,
    MPV_END_FILE_REASON_STOP = 2,
    MPV_END_FILE_REASON_QUIT = 3,
    MPV_END_FILE_REASON_ERROR = 4,
    MPV_END_FILE_REASON_REDIRECT = 5,
}

/// Raw client context.
#[allow(non_camel_case_types)]
pub type mpv_handle = c_void;

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct mpv_event_property {
    pub name: *const c_char,
    pub format: i32,
    pub data: *mut c_void,
}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct mpv_event_log_message {
    pub prefix: *const c_char,
    pub level: *const c_char,
    pub text: *const c_char,
    pub log_level: mpv_log_level,
}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct mpv_event_start_file {
    pub playlist_entry_id: c_ulonglong,
}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct mpv_event_end_file {
    pub reason: mpv_end_file_reason,
    pub error: c_int,
    pub playlist_entry_id: c_ulonglong,
    pub playlist_insert_id: c_ulonglong,
    pub playlist_insert_num_entries: c_int,
}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct mpv_event_client_message {
    pub num_args: c_int,
    pub args: *const *const c_char,
}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct mpv_event_hook {
    pub name: *const c_char,
    pub id: c_ulonglong,
}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct mpv_event {
    pub event_id: mpv_event_id,
    pub error: mpv_error,
    pub reply_userdata: c_ulonglong,
    pub data: *mut c_void,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union Data {
    /// valid if `format` == [`MPV_FORMAT_STRING`](mpv_format::MPV_FORMAT_STRING)
    pub string: *const c_char,
    /// valid if `format` == [`MPV_FORMAT_FLAG`](mpv_format::MPV_FORMAT_FLAG)
    pub flag: c_int,
    /// valid if `format` == [`MPV_FORMAT_INT64`](mpv_format::MPV_FORMAT_INT64)
    pub int64: c_longlong,
    /// valid if `format` == [`MPV_FORMAT_DOUBLE`](mpv_format::MPV_FORMAT_DOUBLE)
    pub double_: c_double,
    /// valid if `format` == [`MPV_FORMAT_NODE_ARRAY`](mpv_format::MPV_FORMAT_NODE_ARRAY)
    ///    or if `format` == [`MPV_FORMAT_NODE_MAP`](mpv_format::MPV_FORMAT_NODE_MAP)
    pub list: *mut mpv_node_list,
    /// valid if `format` == [`MPV_FORMAT_BYTE_ARRAY`](mpv_format::MPV_FORMAT_BYTE_ARRAY)
    pub ba: *mut mpv_byte_array,
}

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
pub struct mpv_node {
    /// Type of the data stored in this struct. This value rules what members in
    /// the given union can be accessed. The following formats are currently
    /// defined to be allowed in [`mpv_node`]:
    ///
    ///  - MPV_FORMAT_STRING       (u.string)
    ///  - MPV_FORMAT_FLAG         (u.flag)
    ///  - MPV_FORMAT_INT64        (u.int64)
    ///  - MPV_FORMAT_DOUBLE       (u.double_)
    ///  - MPV_FORMAT_NODE_ARRAY   (u.list)
    ///  - MPV_FORMAT_NODE_MAP     (u.list)
    ///  - MPV_FORMAT_BYTE_ARRAY   (u.ba)
    ///  - MPV_FORMAT_NONE         (no member)
    ///
    /// If you encounter a value you don't know, you must not make any
    /// assumptions about the contents of union u.
    pub u: Data,
    pub format: mpv_format,
}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct mpv_node_list {
    /// Number of entries. Negative values are not allowed.
    pub num: c_int,
    /// MPV_FORMAT_NODE_ARRAY:
    ///  values[N] refers to value of the Nth item
    ///
    /// MPV_FORMAT_NODE_MAP:
    ///  values[N] refers to value of the Nth key/value pair
    ///
    /// If num > 0, values[0] to values[num-1] (inclusive) are valid.
    /// Otherwise, this can be NULL.
    pub values: *mut mpv_node,
    /// MPV_FORMAT_NODE_ARRAY:
    ///  unused (typically NULL), access is not allowed
    ///
    /// MPV_FORMAT_NODE_MAP:
    ///  keys[N] refers to key of the Nth key/value pair. If num > 0, keys[0] to
    ///  keys[num-1] (inclusive) are valid. Otherwise, this can be NULL.
    ///  The keys are in random order. The only guarantee is that keys[N] belongs
    ///  to the value values[N]. NULL keys are not allowed.
    pub keys: *const *const c_char,
}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct mpv_byte_array {
    /// Pointer to the data. In what format the data is stored is up to whatever
    /// uses MPV_FORMAT_BYTE_ARRAY.
    pub data: *mut c_void,
    /// Size of the data pointed to by ptr.
    pub size: usize,
}

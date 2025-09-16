macro_rules! define_errors {
    ($module:ident => { $($name:ident($code:literal, $msg:literal),)* }) => {
        pub(crate) mod $module {
            $(
                #[allow(clippy::zero_prefixed_literal)]
                pub const $name: crate::error::CssErrorCode =
                     crate::error::CssErrorCode::new_internal($code, $msg);

            )*
        }
    };
}

define_errors!(basic => {
    UNEXPECTED_TOKEN(01, "Unexpected token"),
    BASIC_PARSE_ERROR(02, "Unknown error while parsing"),
    INVALID_AT_RULE(03, "Invalid at-rule"),
    INVALID_SELECTOR(04, "Invalid selector"),
    PROPERTY_NOT_RECOGNIZED(05, "Property not recognized"),
    NON_PARSEABLE_TYPE(06, "Property of type not parseable"),
    UNEXPECTED_FONT_FACE_PROPERTY(07, "Invalid @font-face property"),
    INCOMPLETE_FONT_FACE_RULE(08, "Incomplete @font-face rule"),
    DUPLICATED_KEYFRAMES_ANIMATION(09, "Duplicated @keyframes rule"),
});

define_errors!(animations => {
    INVALID_DURATION(10, "Invalid duration"),
    NONE_EXISTING_ANIMATION(11, "Animation with this name does not exist"),
    INVALID_EASING_FUNCTION_KEYWORD(12, "Invalid easing function keyword"),
    INVALID_EASING_FUNCTION_NAME(13, "Invalid easing function name"),
    INVALID_EASING_FUNCTION_TOKEN(14, "Unexpected easing function token"),
    INVALID_ITERATION_COUNT(15, "Invalid iteration count"),
    INVALID_ANIMATION_DIRECTION(16, "Invalid animation direction"),
    INVALID_STEP_POSITION(17, "Invalid step position keyword"),
    UNEXPECTED_KEYFRAME_TOKEN(18, "Invalid keyframe token"),
});

define_errors!(color => {
    CURRENT_COLOR_NOT_SUPPORTED(20, "'current_color' as a color is not supported"),
    UNSUPPORTED_COLOR_SPACE(21, "Unsupported color space"),
});

define_errors!(enums => {
    INVALID_ENUM_VALUE(30, "Invalid property value"),
});

define_errors!(image => {
    UNEXPECTED_IMAGE_MODE_TOKEN(40, "Unexpected image mode token"),
    UNEXPECTED_RILED_TOKEN(41, "Unexpected token for tiled()"),
});

define_errors!(grid => {
    INVALID_TRACK_TOKEN(50, "Invalid track token"),
    INVALID_TRACK_DIMENSION(51, "Invalid grid track dimension"),
    INVALID_REPETITION_TOKEN(52, "Invalid repetition token"),
    INVALID_FIT_CONTENT_TOKEN(53, "Invalid 'fit-content' token"),
    GRID_PLACEMENT_ZERO_VALUE(54, "Grid placement with value zero"),
    OUTSIDE_OF_RANGE_NUMBER(55, "Outside of range number"),
});

define_errors!(ui => {
    UNEXPECTED_VAL_TOKEN(60, "Unexpected token for a Val type"),
    UNEXPECTED_F32_TOKEN(61, "Unexpected token for a f32 type"),
    INVALID_NUMBER_OF_SHADOW_VALS(62, "Invalid number of values for BoxShadow"),
    UNEXPECTED_LINE_HEIGHT_TOKEN(63, "Unexpected token for a LineHeight type"),
    UNEXPECTED_ANGLE_TOKEN(64, "Unexpected token for an angle type"),
    INVALID_COLOR_SPACE(65, "Invalid color space"),
    INVALID_GRADIENT_FUNCTION(65, "Unexpected gradient function"),
    UNEXPECTED_UI_POSITION_TOKEN(66, "Unexpected ui position token"),
    UNEXPECTED_RADIAL_SHAPE_TOKEN(67, "Unexpected radial shape/size token"),
    MISSING_GRADIENT_COLORS(68, "Unexpected radial shape/size token"),
});

define_errors!(transform => {
    INVALID_TRANSFORM_FUNCTION_ORDER(70, "Transform function not in order"),
    UNEXPECTED_TRANSFORM_FUNCTION(71, "Transform function not supported"),
});

define_errors!(vars => {
    INVALID_TOKEN(80, "Invalid token"),
    INVALID_VAR_TOKEN(81, "Invalid var token"),
    INVALID_VAR_NAME(82, "Var names should start with --"),
});

define_errors!(calc => {
    CALC_ERROR(90, "Could not calculate value"),
});

define_errors!(media_queries => {
    UNRECOGNIZED_PROPERTY(100, "Unrecognized media query property"),
    UNPEXPECTED_SIZE_TOKEN(101, "Unexpected token for a media size type"),
    UNPEXPECTED_COLOR_SCHEMA_TOKEN(102, "Unexpected token for color-scheme"),
    UNPEXPECTED_RESOLUTION_TOKEN(103, "Unexpected token for a resolution type"),
});

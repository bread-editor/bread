package editor

// A group of related options. For example, can be used by modes to set options
// that they need
type OptionGroup map[string]interface{}

// A map of option groups, the overarching option structure in Bread
type OptionMap map[string]OptionGroup

package data

// Options are represented as strings, since we don't have any polymorphism :(
type Option string

// A group of related options. For example, can be used by modes to set options
// that they need
type OptionGroup map[string]Option

// A map of option groups, the overarching option structure in Bread
type OptionMap map[string]OptionGroup

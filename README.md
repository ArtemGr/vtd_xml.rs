# vtd_xml.rs [![crate](https://img.shields.io/crates/v/vtd_xml.svg)](https://crates.io/crates/vtd_xml) [![docs](https://docs.rs/vtd_xml/badge.svg)](https://docs.rs/vtd_xml/) ![](https://tokei.rs/b1/github/ArtemGr/vtd_xml.rs) [![patreon](https://img.shields.io/badge/patreon-donate-green.svg)](https://www.patreon.com/user?u=4695668)
Rust VTD-XML wrapper.

VTD-XML **indexes** the XML documents instead of loading them fully into the RAM. This makes for a very fast ~~parsing~~ indexing step and plays nicely with memory mapping. It uses a cloneable cursor to navigate the XML index, which is almost as convenient as having a DOM tree. XPath queries are supported as well (see the unit test), though as of now they lack a high-level Rust wrapper.

Supported environments: Debian, Ubuntu.

Needs the Nightly version of Rust (because the "test" crate is unstable).

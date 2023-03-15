/*
  This project represents a simplified example of the red/green trees used in
  the Rowen library. This example was created using a tutorial from Aleksey
  Kladov on youtube as a guide:
  https://www.youtube.com/playlist?list=PLhb66M_x9UmrqXhQuIpWC5VgTdrGxMx3y
  as well as following the docs found in the rust-analyzer project:
  https://github.com/rust-lang/rust-analyzer

  The primary purpose of this project is to increase my understanding of the
  syntax trees Rowen implements and to play around with something I find
  intersting.
 */

mod green;
mod red;
mod kinds;
mod lexer;
mod green_node_parser;

fn main() {
    println!("Hello, world!");
}

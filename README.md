# Cone for Reddit

Cone for Reddit is a 3D visualization of the Reddit community based on the ConeCanvas widget developed by Symbolian. This project uses the hierarchical structure of Subreddits, threads, and comments to display them in ConeCanvas. Content is provided by a selection of the most interesting Subreddits.

The left hand side of the screen shows a 3d tree view of selected Subreddits, threads and comments. It allows you to navigate and select nodes in the tree by using the arrow keys on the keyboard or by clicking on nodes.

The right hand side shows content corresponding to the item selected on the left.

While you can not change the data in this example, ConeCanvas can also be used to edit and re-order datasets.

## Implementation

This showcase is implemented using ConeCanvas and the [reddit package for Haskell][1]. ConeCanvas renders the cone tree using [Symbolian's Purescript bindings for WebGL][2].

Please get in contact if you are interested in using ConeCanvas for a web site or app.

## License

All rights reserved (c) 2016 Symbolian GmbH

[symbolian.net][3]

[1]: https://hackage.haskell.org/package/reddit
[2]: https://github.com/jutaro/purescript-webgl
[3]: http://symbolian.net/

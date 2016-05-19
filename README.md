# Cone for Reddit

Cone for Reddit is a 3D visualisation of the Reddit community based on the ConeCanvas widget developed by Symbolian. This project uses the inherently hierarchical structure of Subreddits, threads, and comments by loading them in ConeCanvas. Content is provided by a selection of the most interesting Subreddits.

The left hand side of the screen shows a 3d tree view of selected Subreddits, threads and comments. It allows you to navigate and select nodes in this tree. The right hand side shows content related to the item selected on the left.

Navigate the visualisation with the arrow keys on your keyboard or with a mouse. Click on nodes you want to view and the visualisation will move to display them.

## Implementation

This showcase is implemented using ConeCanvas and the [*reddit* package for Haskell][1]. 

## License

All rights reserved (c) 2016 Symbolian GmbH

[1]: https://hackage.haskell.org/package/reddit
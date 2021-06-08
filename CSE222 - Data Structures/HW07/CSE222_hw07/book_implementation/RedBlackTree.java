package CSE222_hw07.book_implementation;

/** RedBlackTree.java
 *  Implements a Red-Black binary search tree
 *  Red-Black trees were invented by Bayer with refinments
 *  (the color convention) introduced by Guibas and Sedgewick.
 *  @author Koffman and Wolfgang
*/


public class RedBlackTree < E
    extends Comparable < E >>
    extends BinarySearchTreeWithRotate < E > {

  /** Nested class to represent a Red-Black node. */
  private static class RedBlackNode < E >
      extends Node < E > {
    // Additional data members
    /** Color indicator. True if red, false if black. */
    private boolean isRed;

    // Constructor
    /** Create a RedBlackNode with the default color of red
        and the given data field.
        @param item The data field
     */
    public RedBlackNode(E item) {
      super(item);
      isRed = true;
    }

    // Methods
    /** Return a string representation of this object.
        The color (red or black) is appended to the
        node�s contents.
        @return String representation of this object
     */
    public String toString() {
      if (isRed) {
        return "Red  : " + super.toString();
      }
      else {
        return "Black: " + super.toString();
      }
    }
  }

  // Data Field
  /** A boolean variable to indicate that the black height
      was reduced by a call to the recursive delete method
      or one of its sub methods.
   */
  private boolean fixupRequired;

  public RedBlackTree() {
      isRedColor = false;
  }

  /** Insert an item into the tree. This is the starter method
    of a recursive process.
    @param item - The item to be inserted
    @return true if item inserted, false if item already in the tree.
   */
  public boolean add(E item) {
    if (root == null) {
      root = new RedBlackNode < E > (item);
      ( (RedBlackNode < E > ) root).isRed = false; // root is black.
      return true;
    }
    else {
      root = add( (RedBlackNode < E > ) root, item);
      ( (RedBlackNode < E > ) root).isRed = false; // root is always black.
      return addReturn;
    }

  }

  /** Recursive add method.
    @param localRoot - The root of the subtree
    @param item - The item to be inserted
    @return  updated local root of the subtree
    @post insertReturn is set false if item is already in the tree
   */
  private Node < E > add(RedBlackNode < E > localRoot, E item) {
    if (item.compareTo(localRoot.data) == 0) {
      // item already in the tree.
      addReturn = false;
      return localRoot;
    }

    else if (item.compareTo(localRoot.data) < 0) {
      // item < localRoot.data.
      if (localRoot.left == null) {
        // Create new left child.
        localRoot.left = new RedBlackNode < E > (item);
        addReturn = true;
        return localRoot;
      }

      else { // Need to search.
        // Check for two red children, swap colors if found.
        moveBlackDown(localRoot);
        // Recursively add on the left.
        localRoot.left = add( (RedBlackNode < E > ) localRoot.left, item);

        // See whether the left child is now red
        if ( ( (RedBlackNode < E > ) localRoot.left).isRed) {

          if (localRoot.left.left != null
              && ( (RedBlackNode < E > ) localRoot.left.left).isRed) {
            // Left-left grandchild is also red.

            // Single rotation is necessary.
            ( (RedBlackNode < E > ) localRoot.left).isRed = false;
            localRoot.isRed = true;
            return rotateRight(localRoot);
          }
          else if (localRoot.left.right != null
                   && ( (RedBlackNode < E > ) localRoot.left.right).isRed) {
            // Left-right grandchild is also red.
            // Double rotation is necessary.
            localRoot.left = rotateLeft(localRoot.left);
            ( (RedBlackNode < E > ) localRoot.left).isRed = false;
            localRoot.isRed = true;
            return rotateRight(localRoot);
          }
        }
        return localRoot;
      }
    }

    else {
      // item > localRoot.data
      if (localRoot.right == null) {
        /**** BEGIN EXERCISE ****/
        // create new right child
        localRoot.right = new RedBlackNode < E > ( (E) item);
        addReturn = true;
        return localRoot;
      }
      else { // need to search
        // check for two red children swap colors
        moveBlackDown(localRoot);
        // recursively insert on the right
        localRoot.right =
            add( (RedBlackNode < E > ) localRoot.right, item);
        // see if the right child is now red
        if ( ( (RedBlackNode<E>) localRoot.right).isRed) {
          if (localRoot.right.right != null
              && ( (RedBlackNode<E>) localRoot.right.right).isRed) {
            // right-right grandchild is also red
            // single rotate is necessary
            ( (RedBlackNode<E>) localRoot.right).isRed = false;
            localRoot.isRed = true;
            return rotateLeft(localRoot);
          }
          else if (localRoot.right.left != null
                   && ( (RedBlackNode<E>) localRoot.right.left).isRed) {
            // left-right grandchild is also red
            // double rotate is necessary
            localRoot.right = rotateRight(localRoot.right);
            ( (RedBlackNode<E>) localRoot.right).isRed = false;
            localRoot.isRed = true;
            return rotateLeft(localRoot);
          }
        }
        return localRoot;
      }
      /**** END EXERCISE ****/
    }
  }

  /** Method to make the two children of the a sub-tree
    black and the localRoot black.
    @param localRoot The root of the sub-tree
   */
  private void moveBlackDown(RedBlackNode < E > localRoot) {
    // see if both children are red
    if (localRoot.left != null && localRoot.right != null
        && ( (RedBlackNode<E>) localRoot.left).isRed
        && ( (RedBlackNode<E>) localRoot.right).isRed) {
      //make them black and myself red
      ( (RedBlackNode<E>) localRoot.left).isRed = false;
      ( (RedBlackNode<E>) localRoot.right).isRed = false;
      localRoot.isRed = true;
    }
  }

  /**** BEGIN EXERCISE ****/
  /** Delete starter method. Removes the given object
          from the binary search tree.
          Post: The object is not in the tree
          @param item - The object to be removed.
          @return The object from the tree that was removed
          or null if the object was not in the tree.
   */
  public E delete(E item) {
    fixupRequired = false;
    if (root == null) {
      return null;
    }
    else {
      int compareReturn = item.compareTo(root.data);
      if (compareReturn == 0) {
        E oldValue = root.data;
        root = findReplacement(root);
        if (fixupRequired) {
          root = fixupLeft(root);
        }
        return oldValue;
      }
      else if (compareReturn < 0) {
        if (root.left == null) {
          return null;
        }
        else {
          E oldValue = removeFromLeft(root, item);
          if (fixupRequired) {
            root = fixupLeft(root);
          }
          return oldValue;
        }
      }
      else {
        if (root.right == null) {
          return null;
        }
        else {
          E oldValue = removeFromRight(root, item);
          if (fixupRequired) {
            root = fixupRight(root);
          }
          return oldValue;
        }
      }
    }
  }

  /** Recursive remove from left sub-tree method.
      Removes the given object from the binary search tree.
      Pre: The values of parent and parent.left are are not null
      The object is less than parent.data
      @post The object is removed from the tree
      @param parent - Parent of the root of the subtree
      @param item - The object to be removed
      @return The object that was removed or null
      if the item is not in the tree
   */
  private E removeFromLeft(Node < E > parent, E item) {
    if (item.compareTo(parent.left.data) < 0) {
      if (parent.left.left == null) {
        return null;
      }
      else {
        E oldValue = removeFromLeft(parent.left, item);
        if (fixupRequired) {
          parent.left = fixupLeft(parent.left);
        }
        return oldValue;
      }
    }
    else if (item.compareTo(parent.left.data) > 0) {
      if (parent.left.right == null) {
        return null;
      }
      else {
        E oldValue = removeFromRight(parent.left, item);
        if (fixupRequired) {
          parent.left = fixupRight(parent.left);
        }
        return oldValue;
      }
    }
    else {
      E oldValue = parent.left.data;
      parent.left = findReplacement(parent.left);
      if (fixupRequired) {
        parent.left = fixupLeft(parent.left);
      }
      return oldValue;
    }
  }

  /** Recursive remove from right sub-tree method.
      Removes the given object from the binary search tree.
      @pre The values of parent and parent.right are not null
      The object is greater than parent.data
      @post The object is removed from the tree
      @param parent - Parent of the root of the subtree
      @param item - The object to be removed
      @return The object that was removed or null
      if the item is not in the tree
   */
  private E removeFromRight(Node < E > parent, E item) {
    if (item.compareTo(parent.right.data) < 0) {
      if (parent.right.left == null) {
        return null;
      }
      else {
        E oldValue = removeFromLeft(parent.right, item);
        if (fixupRequired) {
          parent.right = fixupLeft(parent.right);
        }
        return oldValue;
      }
    }
    else if (item.compareTo(parent.right.data) > 0) {
      if (parent.right.right == null) {
        return null;
      }
      else {
        E oldValue = removeFromRight(parent.right, item);
        if (fixupRequired) {
          parent.right = fixupRight(parent.right);
        }
        return oldValue;
      }
    }
    else {
      E oldValue = parent.right.data;
      parent.right = findReplacement(parent.right);
      if (fixupRequired) {
        parent.right = fixupLeft(parent.right);
      }
      return oldValue;
    }
  }

  /** Function to find a replacement for a node that is being
      deleted from a binary search tree.  If the node has a null
      child, then the replacement is the other child.  If neither
      are null, then the replacement is the largest value less
      than the item being removed.
      @pre  node is not null
      @post a node is deleted from the tree
      @param node The node to be deleted or replaced
      @return null if both of node's children are null
      node.left if node.right is null
      node.right if node.left is null
      modified copy of node with its data field changed
   */
  private Node < E > findReplacement(Node < E > node) {
    if (node.left == null) {
      if ( ( (RedBlackNode < E > ) node).isRed) {
        // can always remove a red node
        return node.right;
      }
      else if (node.right == null) {
        // We are removing a black leaf
        fixupRequired = true;
        return null;
      }
      else if ( ( (RedBlackNode < E > ) node.right).isRed) {
        // replace black node with red child
        ( (RedBlackNode < E > ) node.right).isRed = false;
        return node.right;
      }
      else {
        // a black node cannot have only one black child
        throw new RuntimeException("Invalid Red-Black Tree Structure");
      }
    }
    else if (node.right == null) {
      if ( ( (RedBlackNode < E > ) node).isRed) {
        // can always remove a red node
        return node.left;
      }
      else if ( ( (RedBlackNode < E > ) node.left).isRed) {
        ( (RedBlackNode < E > ) node.left).isRed = false;
        return node.left;
      }
      else {
        // a black node cannot have only one black child
        throw new RuntimeException("Invalid Red-Black Tree structure");
      }
    }
    else {
      if (node.left.right == null) {
        node.data = node.left.data;
        if ( ( (RedBlackNode < E > ) node.left).isRed) {
          node.left = node.left.left;
        }
        else if (node.left.left == null) {
          fixupRequired = true;
          node.left = null;
        }
        else if ( ( (RedBlackNode < E > ) node.left.left).isRed) {
          ( (RedBlackNode < E > ) node.left.left).isRed = false;
          node.left = node.left.left;
        }
        else {
          throw new
              RuntimeException("Invalid Red-Black Tree structure");
        }
        return node;
      }
      else {
        node.data = findLargestChild(node.left);
        if (fixupRequired) {
          node.left = fixupRight(node.left);
        }
        return node;
      }
    }
  }

  /** Find the node such that parent.right.right == null
      @post The found node is removed from the tree and replaced
      by its left child (if any)
      @param parent - The possible parent
      @return the value of the found node
   */
  private E findLargestChild(Node < E > parent) {
    if (parent.right.right == null) {
      E returnValue = parent.right.data;
      if ( ( (RedBlackNode < E > ) parent.right).isRed) {
        parent.right = parent.right.left;
      }
      else if (parent.right.left == null) {
        fixupRequired = true;
        parent.right = null;
      }
      else if ( ( (RedBlackNode < E > ) parent.right.left).isRed) {
        ( (RedBlackNode < E > ) parent.right.left).isRed = false;
        parent.right = parent.right.left;
      }
      else {
        throw new RuntimeException("Invalid Red-Black Tree structure");
      }
      return returnValue;
    }
    else {
      E returnValue = findLargestChild(parent.right);
      if (fixupRequired) {
        parent.right = fixupRight(parent.right);
      }
      return returnValue;
    }
  }

  /** Method to restore black balance to a subtree whose right black
      height is currently one less than the left black height.
      @param localRoot - The root of the tree needing fixing
      @return A new local root
   */
  private Node < E > fixupRight(Node < E > localRoot) {
    // If local root is null, problem needs to be fixed at
    // the next level -- just return
    if (localRoot == null)return localRoot;
    if ( ( (RedBlackNode < E > ) localRoot).isRed) {
      // If the local root is red, then make it black
      ( (RedBlackNode < E > ) localRoot).isRed = false;
      // If the local root has red left-right grand child
      if (localRoot.left.right != null
          && ( (RedBlackNode < E > ) localRoot.left.right).isRed) {
        // Rotate left child left
        localRoot.left = rotateLeft(localRoot.left);
        // That grandchild is now the child
        // Rotate the localRoot right
        // Fixup is complete after the next step
        fixupRequired = false;
        return rotateRight(localRoot);
      }
      else if (localRoot.left.left != null
               && ( (RedBlackNode < E > ) localRoot.left.left).isRed) {
        // There is a left left grandchild
        // Set it black
        ( (RedBlackNode < E > ) localRoot.left.left).isRed = false;
        // Set child red
        ( (RedBlackNode < E > ) localRoot.left).isRed = true;
        // Fixup is complete after the next step
        fixupRequired = false;
        return rotateRight(localRoot);
      }
      else { // left child is a leaf or has two black children
        // Set it red
        ( (RedBlackNode < E > ) localRoot.left).isRed = true;
        // Fixup is complete
        fixupRequired = false;
        return localRoot;
      }
    }
    else { // localRoot is black
      if ( ( (RedBlackNode < E > ) localRoot.left).isRed) {
        // left child is red
        // set the local root red, and the
        // left child black
        ( (RedBlackNode < E > ) localRoot).isRed = true;
        ( (RedBlackNode < E > ) localRoot.left).isRed = false;
        // Rotate the left child left
        localRoot.left = rotateLeft(localRoot.left);
        Node < E > temp = rotateRight(localRoot);
        // After the next step, the black height of
        // this subtree has been reduced
        // Do not reset fixupRequired
        return rotateRight(temp);
      }
      else {
        // left child is black, set it red
        // do not reset fixupRequired
        ( (RedBlackNode<E>) localRoot.left).isRed = true;
        return localRoot;
      }
    }
  }

  /** Method to restore black balance to a subtree whose left black
      height is currently one less than the right black height.
      @param localRoot - The root of the tree needing fixing
      @return A new local root
   */
  private Node < E > fixupLeft(Node < E > localRoot) {
    // If local root is null, problem needs to be fixed at
    // the next level -- just return
    if (localRoot == null)return localRoot;
    if ( ( (RedBlackNode < E > ) localRoot).isRed) {
      // If the local root is red, then make it black
      ( (RedBlackNode < E > ) localRoot).isRed = false;
      // If the local root has a red right-left grand child
      if (localRoot.right.left != null
          && ( (RedBlackNode < E > ) localRoot.right.left).isRed) {
        // Rotate right child right
        localRoot.right = rotateRight(localRoot.right);
        // That grandchild is now the child
        // Rotate the localRoot left
        // Fixup is complete after the next step
        fixupRequired = false;
        return rotateLeft(localRoot);
      }
      else if (localRoot.right.right != null
               && ( (RedBlackNode < E > ) localRoot.right.right).isRed) {
        // There is a red right right grandchild
        // Set it black
        ( (RedBlackNode < E > ) localRoot.right.right).isRed = false;
        // Set child red
        ( (RedBlackNode < E > ) localRoot.right).isRed = true;
        // Fixup is complete after the next step
        fixupRequired = false;
        return rotateLeft(localRoot);
      }
      else { // right child is a leaf or has two black children
        // Set it red
        ( (RedBlackNode < E > ) localRoot.right).isRed = true;
        // Fixup is complete
        fixupRequired = false;
        return localRoot;
      }
    }
    else { // localRoot is black
      if ( ( (RedBlackNode < E > ) localRoot.right).isRed) {
        // right child is red
        // set the local root red, and the
        // right child black
        ( (RedBlackNode < E > ) localRoot).isRed = true;
        ( (RedBlackNode < E > ) localRoot.right).isRed = false;
        // Rotate the right child right
        localRoot.right = rotateRight(localRoot.right);
        Node < E > temp = rotateLeft(localRoot);
        // After the next step, the black height of
        // this subtree has been reduced
        // Do not reset fixupRequired
        return rotateLeft(temp);
      }
      else {
        // right child is black, set it red
        // and do not reset fixupRequired
        ( (RedBlackNode < E > ) localRoot.right).isRed = true;
        return localRoot;
      }
    }
  }

  /**** END EXERCISE ****/

}

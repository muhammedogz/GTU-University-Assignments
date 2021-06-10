/**
 * @author Muhammed Oguz
 * 
 * This class tests a binarySearchTree type RDB or AVL 
 */

package CSE222_hw07.src_oguz;


import CSE222_hw07.book_implementation.BinarySearchTree;
import CSE222_hw07.book_implementation.BinaryTree;

@SuppressWarnings("rawtypes") // does not matter type.
public class TreeType {
    
    /**
     * Determine given BinarySearchTree is Red Black Tree
     * @param tree The Tree that going to checked
     * @return true if rdb, false otherwise
     */
    public static boolean isRedBlackTree(BinarySearchTree tree) {
        if (tree.isRed() == false)
            return true;
        return false;
    }

    /**
     * Check, given tree is AVL tree
     * @param tree The tree that going to checked
     * @return true if AVL, false otherwise
     */
    public static boolean isAVLTree(BinarySearchTree tree) {
        BinaryTree.Node root = tree.getRootNode();

        return isBalanced(root);
    }

    /**
     * Check nodes is balanced or not recursively
     * @param node The node that going to checked
     * @return true if balanced, false otherwise
     */
    private static boolean isBalanced(BinaryTree.Node node) {

        if (node == null)
            return true;

        int heightLeft = height(node.getLeft());
        int heightRight = height(node.getRight());

        if (Math.abs(heightRight - heightLeft) <= 1 
        && isBalanced(node.getLeft()) && isBalanced(node.getRight()))
            return true;

        return false;
    }

    /**
     * Determine a node's height recursively
     * @param node Node going to checked height
     * @return height, if null return 0
     */
    private static int height(BinaryTree.Node node) {
        if (node == null)
            return 0;

        return 1 + Math.max(height(node.getLeft()), height(node.getRight()));

    }
}

package CSE222_hw07.src_oguz;


import CSE222_hw07.book_implementation.BinarySearchTree;
import CSE222_hw07.book_implementation.BinaryTree;

@SuppressWarnings("rawtypes") // does not matter type.
public class TreeType {
    
    public static boolean isRedBlackTree(BinarySearchTree tree) {
        if (tree.isRed() == false)
            return true;
        return false;
    }

    public static boolean isAVLTree(BinarySearchTree tree) {
        BinaryTree.Node root = tree.getRootNode();

        return isBalanced(root);
    }

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

    private static int height(BinaryTree.Node node) {
        if (node == null)
            return 0;

        return 1 + Math.max(height(node.getLeft()), height(node.getRight()));

    }
}

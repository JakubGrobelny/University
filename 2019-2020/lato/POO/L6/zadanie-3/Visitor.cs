using System;

namespace Zadanie3 {

    public abstract class Tree<T> {
    }

    public class TreeNode<T> : Tree<T> {
        public Tree<T> Left  { get; set; }
        public Tree<T> Right { get; set; }    
        
        public TreeNode(Tree<T> left, Tree<T> right) {
            this.Left = left;
            this.Right = right;
        }
    }

    public class TreeLeaf<T> : Tree<T> {
        public T Value { get; set; }

        public TreeLeaf(T val) {
            this.Value = val;
        }
    }

    public abstract class TreeVisitor<T, Val> {
        public Val Visit(Tree<T> tree) {
            if (tree is TreeNode<T> node) {
                return this.VisitNode(node);
            } else {
                return this.VisitLeaf((TreeLeaf<T>)tree);
            }
        }

        public abstract Val VisitNode(TreeNode<T> node);

        public abstract Val VisitLeaf(TreeLeaf<T> leaf);
    }

    public class TreeHeightVisitor<T> : TreeVisitor<T, int> {
        public override int VisitNode(TreeNode<T> node) {
            return Math.Max(Visit(node.Left), Visit(node.Right)) + 1;
        }

        public override int VisitLeaf(TreeLeaf<T> leaf) {
            return 0;
        }
    }

    public class Example {
        public static void Main() { 
            var tree = new TreeNode<int>(  // 1
                new TreeNode<int>(         // 2
                    new TreeLeaf<int>(0),
                    new TreeNode<int>(     // 3
                        new TreeNode<int>( // 4
                            new TreeLeaf<int>(2),
                            new TreeLeaf<int>(3)
                        ),
                        new TreeLeaf<int>(70)
                    )
                ),
                new TreeLeaf<int>(42)
            );

            var visitor = new TreeHeightVisitor<int>();
            var height = visitor.Visit(tree);

            Console.WriteLine("Tree height: {0}", height);
        }
    }
}
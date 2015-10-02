import java.util.*;
class BST
{
	Node root;

	public void addNode(int key)
	{
		Node newNode = new Node(key);
		if(root==null)
		{
			root = newNode;
		}
		else
		{
			Node focusNode =root;
			Node parent;
			while(true)
			{
				parent =focusNode;
				if(key < focusNode.key)
				{
					focusNode = focusNode.leftChild;
					if(focusNode==null)
					{
						parent.leftChild = newNode;
						return;
					}
				}
				else
				{
					focusNode = focusNode.rightChild;
					if(focusNode==null)
					{
						parent.rightChild = newNode;
						return;
					}
				}
			}
		}
	}
	public void traverseTree(BST tree,int k)
	{
		if(k==1)
		{
			preorder(tree.root);
		}
		else if(k==2)
		{
			inorder(tree.root);
		}
		else if(k==3)
		{
			postorder(tree.root);
		}
	}
	void preorder(Node root)
	{
		if(root!=null)
		{
			System.out.print(root.key+" ");
			preorder(root.leftChild);
			preorder(root.rightChild);
		}
	}
	ArrayList<Integer> inorder(Node root)
	{
		ArrayList<Integer> temp = new ArrayList<Integer>();
		if(root!=null)
		{
			temp.addAll(0,inorder(root.leftChild));
			System.out.print(root.key+" ");
			temp.add(root.key);
			temp.addAll(temp.size(),inorder(root.rightChild));
		}
		return temp;
	}
	void postorder(Node root)
	{
		if(root!=null)
		{
			postorder(root.leftChild);
			postorder(root.rightChild);
			System.out.print(root.key+" ");
		}
	}
	public boolean isBalanced(BST tree)
	{
			if(tree.root==null)
			{
				return true;
			}
			int left = getLeftHeight(tree.root.leftChild);
			int right = getRightHeight(tree.root.rightChild);
			System.out.println("Left : "+left+" Right : "+right);
			if(left-right == -1 || left-right == 0 || left-right == 1)
			{
				return true;
			}
			else
			{
				return false;
			}
	}
	public int getLeftHeight(Node n)
	{
		if(n==null)
		{
			return 0;
		}
		else
		{
			int k =  getLeftHeight(n.leftChild);
			if(k==0)
			{
				return getLeftHeight(n.rightChild)+1;
			}
			else 
			{
				return (k+1);
			}
		}
	}
	public int getRightHeight(Node n)
	{
		if(n==null)
		{
			return 0;
		}
		else
		{
			int k =  getRightHeight(n.rightChild);
			if(k==0)
			{
				return getRightHeight(n.leftChild)+1;
			}
			else 
			{
				return (k+1);
			}
		}
	}
	public boolean isBalancedCorrect(BST tree)
	{
		Node root = tree.root;
		if(checkHeight(root)==-1)
		{
			return false;
		}
		else
		{
			return true;
		}
	}
	public int checkHeight(Node root)
	{
		if(root==null)
		{
			return 0;
		}
		int leftHeight = checkHeight(root.leftChild);
		if(leftHeight==-1)
		{
			return -1;
		}
		int rightHeight = checkHeight(root.rightChild);
		if(rightHeight==-1)
		{
			return -1;
		}
		int heightDiff = leftHeight - rightHeight;
		if(Math.abs(heightDiff)>1)
		{
			return -1;
		}
		else
		{
			return Math.max(leftHeight,rightHeight)+1;
		}
	}
	public BST balanceTree(BST tree)
	{
		ArrayList<Integer> tmp = new ArrayList<Integer>();
		BST balanced = new BST();
		tmp = inorder(tree.root);
		Node root = new Node(tmp.get(tmp.size()/2));
		balanced.root=root;
		for(int i=(tmp.size()/2)-1;i>=0;i--)
		{
			balanced.addNode(tmp.get(i));
		}
		for(int i = (tmp.size()/2)+1;i<tmp.size();i++)
		{
			balanced.addNode(tmp.get(i));
		}
		System.out.println("Root is : "+root.key);
		return balanced;
	}
	public boolean lookUp(int key)
	{
		return ifExist(root,key);
	}
	public boolean ifExist(Node root,int key)
	{
		if(root==null)
		{
			return false;
		}
		else
		{
			if(key==root.key)
			{
				return true;
			}
			else if(key < root.key)
			{
				return ifExist(root.leftChild,key);
			}
			else
			{
				return ifExist(root.rightChild,key);
			}
		}
	}
	public BST delete(BST tree,int key)
	{
		Node root = tree.root;
		if(root==null)
		{
			System.out.println("tree is emprty!");
			return tree;
		}
		else
		{
			tree.root = deleteNode(root,key);
		}
		return tree;
	}
	public Node deleteNode(Node root,int key)
	{
		Node parent = root;
		if(root.key==key)
		{
			if(root.leftChild==null && root.rightChild==null)
			{
				return null;
			}
			else if(root.leftChild!=null && root.rightChild==null)
			{
				return root.leftChild;
			}
			else if(root.leftChild==null && root.rightChild!=null)
			{
				return root.rightChild;
			}
			else
			{
				parent.key = getMin(root.rightChild);
				parent.rightChild = deleteNode(parent.rightChild,parent.key);
				return parent;
			}
		}
		else if(root.key > key)
		{
			parent.leftChild = deleteNode(root.leftChild,key);
		}
		else
		{
			parent.rightChild = deleteNode(root.rightChild,key);
		}
		return parent;
	}
	public int getMin(Node root)
	{
		Node node = root;
		while(node.leftChild!=null)
		{
				 node = node.leftChild;
		}
		return node.key; 
	}
	public ArrayList<LinkedList<Node>> treeToList(BST tree)
	{
		ArrayList<LinkedList<Node>> result = new ArrayList<LinkedList<Integer>>();
		LinkedList<Node> current = new LinkedList<Node>();
		if(tree.root!=null)
		{
			current.add(tree.root)
		}
		while(current.size()>0)
		{
			result.add(current);
			LinkedList<Node> parent = current;
			current = new LinkedList<Node>();
			for(Node n : parent)
			{
				if(n.leftChild!=null)
				{
					current.add(n.leftChild);
				}
				if(n.rightChild!=null)
				{
					current.add(n.rightChild);
				}
			}
		}
	}
	public static void main(String args[])
	{
		BST tree = new BST();
		tree.addNode(45);
		tree.addNode(415);
		tree.addNode(145);
		tree.addNode(455);
		//tree.addNode(545);
		tree.addNode(345);
		tree.addNode(435);
		tree.addNode(25);
		tree.addNode(15);
		tree.addNode(18);
		tree.addNode(27);
		tree.addNode(100);
		tree.traverseTree(tree,1);
		System.out.println();
		tree.traverseTree(tree,2);
		System.out.println();
		tree.traverseTree(tree,3);
		System.out.println();
		System.out.println("tree balanced : "+tree.isBalancedCorrect(tree));
		//tree = tree.balanceTree(tree);
		//System.out.println("tree After balancing : "+tree.isBalanced(tree));
		System.out.println("Look up for 145 : "+tree.lookUp(145));
		System.out.println("Look up for 25 : "+tree.lookUp(25));
		System.out.println("Look up for 545 : "+tree.lookUp(545));
		tree = tree.delete(tree,145);
		System.out.println("Look up for 145 : "+tree.lookUp(145));
		System.out.println();
		tree.traverseTree(tree,2);
	}

}
class Node
{
	Node leftChild;
	Node rightChild;
	int key;

	public Node(int k)
	{
		key = k;
	}
}
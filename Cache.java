import java.util.*;
//This program creates a cache of user-defined capacity and can be used with three different possible options - 1. Least Recently Used 2. Most Recently Use 3. Least Frequently Used 
public class Cache<K,V>
{
    //Class Node of the linked list that we matain for our cache with key-value pair and the frequency of the item used
	class Node<K,V>
	{
		K key;
		V value;
		int count;
		Node<K,V> previous; // The link to the previous node if exists else null
		Node<K,V> next; //The line to the next node if exists else null

		public Node(K key, V value,Node<K,V> previous,Node<K,V> next)
		{
			key = key;
			value = value;
			count = 1;
			previous = previous;
			next = next;
		}
	}
    // Member variables of the Cache class
	private String type;
	private int maxSize; //user-defined maximum capacity of the cache
	private int currentSize; // current size of the cache
	private Node<K,V> LRU;
	private Node<K,V> MRU;
	private HashMap<K,Node<K,V>> tracker; // a hashmap is implemented to keep the node finding task to constant i.e. Big-O of 1.

	public Cache(int maxSize,String type)
	{
		type = type;
        this.maxSize = maxSize;
        this.currentSize = 0;
        LRU = new Node<K,V>(null, null, null, null);
        MRU = LRU;
        tracker = new HashMap<K, Node<K, V>>();
    }
    //The below function puts a new entry to the cache 
    public void put(K key, V value)
    {
        //Check if the entry already exists in the Cache
    	if(tracker.containsKey(key))
    	{ 
    		updateCache(key); // Update its last use value by placing it to the front end of the linked list
    		return;
    	}
        //if the entry is not present in the cache we add it to the cache and increment the cache currentSize
        Node<K,V> newNode = new Node<K,V>(key,value,MRU,null);
    	currentSize++;
    	if(currentSize==1)
    	{
    		LRU = newNode;
    	}
        //if the size after addition will exceed we remove the element based on the selected algorithm 
    	else if(currentSize>maxSize)
    	{
            //Least recently used item is removed
    		if(type.equalsIgnoreCase("LRU"))
    		{
    			tracker.remove(LRU.key);
    			LRU = LRU.next;
    			LRU.previous = null;
    			currentSize--;
    		}
            //Most recently used item is removed
    		else if(type.equalsIgnoreCase("MRU"))
    		{
    			tracker.remove(MRU.key);
    			MRU = MRU.previous;
    			MRU.next = null;
    			currentSize--;
    		}
            //Least frequently used item is found and placed on the front end of the linked list and then removed before adding the new entry
    		else if(type.equalsIgnoreCase("LFU"))
    		{
				K leastKey = getLeastFrequent(); //finding the least frequently used item
	    		updateCache(leastKey);
	    		tracker.remove(MRU.key);
    			MRU = MRU.previous;
    			MRU.next = null;
    			currentSize--;
    		}
    	}
        newNode = new Node<K,V>(key,value,MRU,null);
    	MRU.next = newNode;
    	MRU = newNode;
    	tracker.put(key,newNode);    	
    }

    //The below function updates the linked list and the hashmap with the latest used entry in the Cache and places it at the front end of the list
    public V updateCache(K Key)
    {
    	Node<K,V> temp = tracker.get(Key);
        //Check if the item to be updated is already the most recently used item
    	if(temp.key==MRU.key)
    	{
    		MRU.count++;
    		tracker.put(Key,MRU);
    		return MRU.value;
    	}
        //Check if the item to be updated is the least recently used item
    	else if(temp.key==LRU.key)
    	{
    		LRU = LRU.next;
    		LRU.previous = null;
    		tracker.put(LRU.key,LRU);
    	}
        //Check if the item to be updated exists in between the list somewhere
    	else
    	{
    		Node<K, V> nextNode = temp.next;
	        Node<K, V> previousNode = temp.previous;
	        previousNode.next = nextNode;
	        nextNode.previous = previousNode;
	        tracker.put(nextNode.key,nextNode);
	        tracker.put(previousNode.key,previousNode);
    	}
    	temp.count++;
    	MRU.next = temp;
    	tracker.put(MRU.key,MRU);
    	temp.previous = MRU;
    	temp.next = null;
    	MRU = temp;
    	tracker.put(MRU.key,MRU);
        return temp.value;
    }

    // Get the key of the item with the least frequency of use.
    public K getLeastFrequent()
    {
     	int min = Integer.MAX_VALUE;
     	K key=null;	
     	for(Node<K,V> node : tracker.values())
     	{
     		if(node.count<min)
     		{
     			min = node.count;
     			key = node.key;
     		}
     	}
     	return key;
    }
}
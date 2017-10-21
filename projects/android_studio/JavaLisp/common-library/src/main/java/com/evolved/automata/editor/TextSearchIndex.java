package com.evolved.automata.editor;

import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;

import javax.xml.soap.Text;

/**
 * Created by Evolved8 on 10/2/17.
 */

public class TextSearchIndex {

    public static class IndexItem {
        int _lineItem;
        char _charValue;
        int _sortedPosition;
        IndexItem _nextItem;
        int _position;

        public int getLineNumber()
        {
            return _lineItem;
        }

        public char getStartChar()
        {
            return _charValue;
        }

        public int getPosition()
        {
            return _position;
        }

        public IndexItem getNextItem()
        {
            return _nextItem;
        }
    }


    IndexItem[] mIndexHeap;
    IndexItem[] mSortedIndex;
    String mInput;

    int mHeapSize = 0;

    Comparator<IndexItem> mComparator = new Comparator<IndexItem>()
    {
        public int compare(IndexItem lvalue, IndexItem rvalue)
        {
            if (lvalue._charValue < rvalue._charValue)
                return -1;
            else if (lvalue._charValue > rvalue._charValue)
                return 1;
            else if (lvalue._position < rvalue._position)
                return -1;
            else if (lvalue._position > rvalue._position)
                return 1;
            else
                return 0;

        }
    };

    public TextSearchIndex(String input)
    {
        mInput = input;
        initializeIndex();;
    }

    void initializeIndex()
    {
        mIndexHeap = new IndexItem[mInput.length()+1];
        mHeapSize = 0;

        IndexItem prevItem = null, currentItem;

        int lineNumber = 1;
        for (int i = 0; i < mInput.length();i++)
        {
            currentItem = new IndexItem();
            currentItem._position = i;
            currentItem._charValue = mInput.charAt(i);
            currentItem._lineItem = lineNumber;
            if (prevItem != null)
                prevItem._nextItem = currentItem;
            addIndexItem(currentItem);
            if (currentItem._charValue == '\n')
                lineNumber++;
            prevItem = currentItem;
        }

        mSortedIndex = new IndexItem[mInput.length()];
        int j = 0;
        while (getHeapSize()>0)
        {
            mSortedIndex[j] = pollIndexHeap();
            mSortedIndex[j]._sortedPosition = j;
            j++;
        }
    }

    private int getHeapSize()
    {
        return mHeapSize;
    }

    private int incrementHeapSize()
    {
        mHeapSize++;
        return mHeapSize;
    }

    private int decrementHeapSize()
    {
        mHeapSize--;
        return mHeapSize;
    }


    private boolean addIndexItem(IndexItem item)
    {
        if (getHeapSize() < mInput.length())
        {
            incrementHeapSize();
            mIndexHeap[getHeapSize()] = item;
            heapifyUp(getHeapSize());
            return true;
        }
        return false;
    }

    private IndexItem pollIndexHeap()
    {
        if (getHeapSize() > 0)
        {
            IndexItem out = mIndexHeap[1];

            mIndexHeap[1] = mIndexHeap[getHeapSize()];
            decrementHeapSize();
            heapifyDown(1);
            return out;
        }
        else
            return null;
    }

    private void heapifyUp(int i)
    {
        if (i > 1)
        {
            int parentIndex = i/2;
            IndexItem parent = mIndexHeap[parentIndex];
            IndexItem current = mIndexHeap[i];

            if (mComparator.compare(current, parent) == -1)
            {
                mIndexHeap[parentIndex] = current;
                mIndexHeap[i] = parent;
                heapifyUp(parentIndex);
            }

        }
    }

    private void heapifyDown(int i)
    {
        int leftChildIndex = 2 * i, rightChildIndex = 2*i + 1, childIndex;
        if (leftChildIndex <= getHeapSize())
        {
            IndexItem current = mIndexHeap[i];
            childIndex = leftChildIndex;
            IndexItem child = mIndexHeap[leftChildIndex];


            if (rightChildIndex <= getHeapSize())
            {
                if (mComparator.compare(mIndexHeap[rightChildIndex], child) == -1)
                {
                    child = mIndexHeap[rightChildIndex];
                    childIndex = rightChildIndex;
                }
            }

            if (mComparator.compare(child, current) == -1)
            {
                mIndexHeap[i] = child;
                mIndexHeap[childIndex] = current;
                heapifyDown(childIndex);
            }
        }
    }

    private TextSearchResult convertIndex(final IndexItem item, final String text)
    {
        return new TextSearchResult()
        {

            @Override
            public int getLineNumber()
            {
                return item.getLineNumber();
            }

            @Override
            public int getStartPosition()
            {
                return item.getPosition();
            }

            @Override
            public String getContextualText(int maxCharactersBefore, int maxCharactersAfter)
            {
                int start = Math.max(0, item.getPosition() - maxCharactersBefore);
                int end = Math.min(mInput.length(), item.getPosition() + text.length() + maxCharactersAfter);
                return mInput.substring(start, end);
            }

            @Override
            public void replaceResultText(String newText, ResultHandler onUpdated)
            {
                String prefix = mInput.substring(0, item.getPosition());
                String suffix = mInput.substring(item.getPosition() + text.length());

                TextSearchIndex newIndex = new TextSearchIndex(prefix + newText + suffix);
                onUpdated.onReplaceComplete(newIndex);
            }

            @Override
            public String previewTextReplacement(String replacement, int maxCharactersBefore, int maxCharactersAfter)
            {
                int start = Math.max(0, item.getPosition() - maxCharactersBefore);
                int end = Math.min(mInput.length(), item.getPosition() + text.length() + maxCharactersAfter);

                String prefix = mInput.substring(start, item.getPosition());
                String suffix = mInput.substring(item.getPosition() + text.length(), end);

                return prefix + replacement + suffix;
            }
        };
    }



    private class SearchIterator implements Iterator<TextSearchResult> {

        String _searchText;

        int _searchPosition;

        boolean SORT_RESULTS_BY_ORDER_OF_OCCURRENCE= true;

        boolean _searchingForwardP = true;
        int _initialSearchPosition = -1;

        boolean _hasNextIsValidP = false;
        boolean _cachedHasNext = false;

        SearchIterator(String searchText)
        {

            _searchText = searchText;
            findInitialCharPosition();

        }


        private void findInitialCharPosition()
        {
            if (_searchText.length() == 0)
            {
                _cachedHasNext = false;
                _hasNextIsValidP = true;
                _initialSearchPosition = _searchPosition = -1;
                return;
            }
            char first = _searchText.charAt(0);
            int topIndex = mSortedIndex.length;
            int bottomIndex = 0;
            int midPoint = 0;

            IndexItem testItem;
            while (topIndex - bottomIndex > 0)
            {
                midPoint = (topIndex - bottomIndex)/2 + bottomIndex;
                testItem = mSortedIndex[midPoint];
                if (testItem._charValue == first)
                {
                    if (SORT_RESULTS_BY_ORDER_OF_OCCURRENCE)
                    {
                        while (mSortedIndex[midPoint - 1]._charValue == first)
                        {
                            midPoint--;
                        }
                    }
                    _initialSearchPosition = _searchPosition = midPoint;
                    return;
                }

                if (testItem._charValue < first)
                {
                    bottomIndex = midPoint + 1;
                }
                else
                {
                    topIndex = midPoint;
                }
            }
            _initialSearchPosition = _searchPosition = -1;
        }

        // returns the position != _searchPosition of an IndexItem whose first character is _searchText
        int getNextSearchIndex()
        {

            boolean indexFoundP = false;
            char testChar = _searchText.charAt(0);
            if (_searchingForwardP)
            {

                if (_searchPosition + 1 < mSortedIndex.length   && (indexFoundP = (mSortedIndex[_searchPosition + 1].getStartChar() == testChar)))
                {
                    _searchPosition++;
                    return _searchPosition;
                }

                _searchingForwardP = false;
                _searchPosition = _initialSearchPosition;
            }

            if (!_searchingForwardP)
            {
                indexFoundP = false;
                if (_searchPosition > 0 && (indexFoundP = (mSortedIndex[_searchPosition-1].getStartChar() == testChar)))
                {
                    _searchPosition--;
                    return _searchPosition;
                }

            }
            return _searchPosition = -1;
        }

        // returns true if item is the start of a match for _searchText
        boolean checkIndex(int indexPos)
        {
            IndexItem item = mSortedIndex[indexPos];

            int start = item.getPosition();
            if (start + _searchText.length() <= mInput.length())
            {
                for (int i = 0; i < _searchText.length(); i++)
                {
                    if (_searchText.charAt(i) != mInput.charAt(i + start))
                        return false;
                }
                return true;
            }
            return false;
        }


        @Override
        public boolean hasNext()
        {
            if (_hasNextIsValidP)
                return _cachedHasNext;
            _hasNextIsValidP = true;
            if (_searchPosition == -1)
                return _cachedHasNext = false;
            _cachedHasNext = checkIndex(_searchPosition);
            while (!_cachedHasNext && getNextSearchIndex() != -1)
            {
                _cachedHasNext = checkIndex(_searchPosition);
            }

            return _cachedHasNext;
        }

        @Override
        public TextSearchResult next()
        {
            TextSearchResult result = null;
            if (hasNext())
            {
                result = convertIndex(mSortedIndex[_searchPosition], _searchText);
                _searchPosition = getNextSearchIndex();
                _hasNextIsValidP = false;
            }
            return result;
        }

        @Override
        public void remove()
        {

        }
    }

    public SearchIterator getSearchIterator(String searchText)
    {
        return new SearchIterator(searchText);
    }

    public LinkedList<IndexItem> findString(String searchString)
    {
        LinkedList<IndexItem> resultList = new LinkedList<IndexItem>();
        if (searchString.length() > 0)
        {
            char first = searchString.charAt(0);
            int topIndex = mSortedIndex.length;
            int bottomIndex = 0;
            int midPoint = (topIndex - bottomIndex)/2 + bottomIndex;

            IndexItem initialItem = null, testItem;
            while (topIndex - bottomIndex > 0)
            {
                midPoint = (topIndex - bottomIndex)/2 + bottomIndex;
                testItem = mSortedIndex[midPoint];
                if (testItem._charValue == first)
                {
                    initialItem = testItem;
                    break;
                }

                if (testItem._charValue < first)
                {
                    bottomIndex = midPoint+1;
                }
                else
                {
                    topIndex = midPoint;
                }
            }

            if (initialItem != null)
            {
                int offsetIndex = midPoint;
                while (initialItem != null)
                {
                    IndexItem result = findString(initialItem, searchString);
                    if (result != null)
                        resultList.add(result);
                    offsetIndex++;
                    if (offsetIndex < mSortedIndex.length && mSortedIndex[offsetIndex]._charValue == first)
                    {
                        initialItem = mSortedIndex[offsetIndex];
                    }
                    else
                        initialItem = null;
                }
                offsetIndex = midPoint - 1;
                if (offsetIndex >= 0)
                    initialItem = mSortedIndex[offsetIndex];
                while (initialItem != null)
                {
                    IndexItem result = findString(initialItem, searchString);
                    if (result != null)
                        resultList.add(result);
                    offsetIndex--;
                    if (offsetIndex >= 0 && mSortedIndex[offsetIndex]._charValue == first)
                    {
                        initialItem = mSortedIndex[offsetIndex];
                    }
                    else
                        initialItem = null;
                }
            }
        }
        return resultList;

    }

    private IndexItem findString(IndexItem startIndexItem, String searchItem)
    {
        int baseIndex = startIndexItem._position;
        for (int i = 0; i < searchItem.length();i++)
        {
            if (baseIndex >= mInput.length() || mInput.charAt(baseIndex + i) != searchItem.charAt(i))
                return null;
        }
        return startIndexItem;
    }


}

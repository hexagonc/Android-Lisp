package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.ArrayMapper;
import com.evolved.automata.IndexedValueMapper;

import org.apache.commons.lang3.StringUtils;

import java.util.Comparator;
import java.util.HashMap;
import java.util.PriorityQueue;

/**
 * Created by Evolved8 on 1/9/17.
 */
public class RingBufferManager implements BufferManager {

    private static class Claim
    {
        public long createTime;
        public int id;
        public Claim(int claimId)
        {
            id = claimId;
            createTime = System.currentTimeMillis();
        }

        public Claim(int id, long createTime)
        {
            this.id = id;
            this.createTime = createTime;
        }
    }

    HashMap<Integer, Claim> claimedSlots;


    int maxClaims;

    PriorityQueue<Claim> claimQueue;
    int previousClaimId = -1;

    public static final String _ENTRY_DELIMITER = "<>";

    public Claim[] getAllClaims()
    {
        return claimQueue.toArray(new Claim[0]);
    }

    public RingBufferManager addClaim(Claim c)
    {
        previousClaimId = c.id;
        claimQueue.add(c);
        claimedSlots.put(c.id, c);
        return this;
    }

    public RingBufferManager addAllClaims(Claim[] claimList)
    {
        for (Claim c:claimList)
        {
            addClaim(c);
        }
        return this;
    }

    public String getSerializedForm()
    {

        Claim[] claims = claimQueue.toArray(new Claim[0]);


        final StringBuilder s = new StringBuilder();
        AITools.map(claims, new ArrayMapper<Claim>() {
            @Override
            public Claim map(Claim input, int index)
            {
                if (index > 0)
                {
                    s.append(NNTools._VDELIMITER);
                }

                s.append(input.id).append("**").append(input.createTime);
                return null;
            }
        });
        int cs = getOldestClaimedSlot();

        return s.toString() + _ENTRY_DELIMITER + previousClaimId +_ENTRY_DELIMITER + maxClaims ;
    }

    @Override
    public void freeAllClaims()
    {
        for (int i = 0;i < maxClaims;i++)
        {
            free(i);
        }
    }


    public static RingBufferManager fromSerializedForm(String serialized)
    {
        String[] parts = StringUtils.splitByWholeSeparatorPreserveAllTokens(serialized, _ENTRY_DELIMITER);
        String serializedClaims = parts[0];
        int lastClaim = Integer.parseInt(parts[1]);
        int maxClaims = Integer.parseInt(parts[2]);

        final HashMap<Integer, Claim> claimedSlots = new HashMap<Integer, Claim>();
        final PriorityQueue<Claim> cQueue = new PriorityQueue<Claim>(maxClaims, getClaimComparator());



        final String[] claimParts = StringUtils.splitByWholeSeparatorPreserveAllTokens(serializedClaims, NNTools._VDELIMITER);
        AITools.mapValues(claimParts, new IndexedValueMapper<String, Claim>() {
            @Override
            public Claim map(String input, int index)
            {
                if (input != null)
                {
                    String[] parts = StringUtils.splitByWholeSeparatorPreserveAllTokens(input, "**");
                    int id =Integer.parseInt(parts[0]);
                    long cTime = Long.parseLong(parts[1]);
                    Claim c = new Claim(id, cTime);
                    cQueue.add(c);
                    claimedSlots.put(Integer.valueOf(id), c);
                }

                return null;
            }

            @Override
            public Claim[] getEmptyOutput()
            {
                return new Claim[0];
            }
        });

        RingBufferManager manager = new RingBufferManager(maxClaims);
        manager.claimQueue = cQueue;
        manager.claimedSlots = claimedSlots;
        manager.previousClaimId = lastClaim;
        int cs = manager.getOldestClaimedSlot();
        return manager;
    }


    public RingBufferManager(int maxNumClaims)
    {
        this.maxClaims = maxNumClaims;
        claimQueue = new PriorityQueue<Claim>(maxClaims, getClaimComparator());

        claimedSlots = new HashMap<Integer, Claim>();
    }

    public Claim getClaim(int id)
    {
        return claimedSlots.get(Integer.valueOf(id));
    }

    public static Comparator<Claim> getClaimComparator()
    {
        return new Comparator<Claim>()
        {

            @Override
            public int compare(Claim o1, Claim o2)
            {
                return Long.compare(o1.createTime, o2.createTime);
            }
        };
    }

    @Override
    public int requestUnclaimedSlot()
    {
        int next = -1;
        if (getNumberOfClaimedSlots() < maxClaims)
        {
            for (int i = 0;i<maxClaims;i++)
            {
                if (!claimedSlots.containsKey(Integer.valueOf(i)))
                {
                    next = i;
                    break;
                }
            }
            claimSlot(next);
        }

        return next;
    }

    @Override
    public boolean claimSlot(int id)
    {
        if (id < -1 || id >= maxClaims)
            return false;
        Claim next = new Claim(id);
        Claim previous = claimedSlots.get(Integer.valueOf(id));
        if (previous!=null)
        {
            claimQueue.remove(previous);
        }
        claimQueue.add(next);
        claimedSlots.put(Integer.valueOf(id), next);
        previousClaimId = id;
        return true;
    }

    @Override
    public int getNumberOfUnclaimedSlots()
    {
        return maxClaims - claimQueue.size();
    }

    @Override
    public boolean isUnclaimed(int id)
    {
        return id > claimQueue.size();
    }

    @Override
    public int getOldestClaimedSlot()
    {
        if (claimQueue.size() == 0)
            throw new IllegalStateException("Buffer is empty");
        return claimQueue.peek().id;
    }

    @Override
    public int getLastClaimedSlot()
    {
        return previousClaimId;
    }

    @Override
    public int getNumberOfClaimedSlots()
    {
        return maxClaims - getNumberOfUnclaimedSlots();
    }

    @Override
    public int tryClaimBestSlot()
    {
        int next = requestUnclaimedSlot();
        if (next == -1)
        {
            next = getOldestClaimedSlot();
            refreshClaim(next);
        }

        return next;
    }



    public boolean free(int id)
    {
        if (claimedSlots.containsKey(Integer.valueOf(id)))
        {
            Claim prior = claimedSlots.get(Integer.valueOf(id));
            claimQueue.remove(prior);
            claimedSlots.remove(Integer.valueOf(id));

            return true;
        }
        return false;
    }

    public boolean refreshClaim(int id)
    {
        Integer ckey = Integer.valueOf(id);
        Claim c = claimedSlots.get(ckey);
        if (c != null)
        {
            claimQueue.remove(c);
            c.createTime = System.currentTimeMillis();
            claimQueue.add(c);

            return true;
        }
        return false;
    }


}

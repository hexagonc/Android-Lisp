package com.evolved.automata.nn;

/**
 * Created by Evolved8 on 1/9/17.
 */
public interface BufferManager {
    int requestUnclaimedSlot();
    boolean claimSlot(int id);
    int getNumberOfUnclaimedSlots();
    boolean isUnclaimed(int id);
    int getOldestClaimedSlot();
    int getLastClaimedSlot();
    int tryClaimBestSlot();
    int getNumberOfClaimedSlots();
    String getSerializedForm();
    void freeAllClaims();
}

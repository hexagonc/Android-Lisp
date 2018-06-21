package com.evolved.automata.events;

import java.util.LinkedList;

/**
 * Created by Evolved8 on 7/2/17.
 */

public interface OnPrereqsLostHandler {
    void onLostPrerequesites(LinkedList<String> lost);
}

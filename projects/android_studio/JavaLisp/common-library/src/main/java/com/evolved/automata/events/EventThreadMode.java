package com.evolved.automata.events;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Created by Evolved8 on 7/4/17.
 */

@Retention(RetentionPolicy.RUNTIME)
public @interface EventThreadMode {
    String mode();
}

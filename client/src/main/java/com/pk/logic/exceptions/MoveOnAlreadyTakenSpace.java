package com.pk.logic.exceptions;

public class MoveOnAlreadyTakenSpace extends Exception {
    public MoveOnAlreadyTakenSpace() {
    }

    public MoveOnAlreadyTakenSpace(String errorMessage) {
        super(errorMessage);
    }

    public MoveOnAlreadyTakenSpace(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
    
}

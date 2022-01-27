package com.pk.logic.exceptions;

public class IllegalArgument extends Exception {
    public IllegalArgument() {
    }

    public IllegalArgument(String errorMessage) {
        super(errorMessage);
    }

    public IllegalArgument(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}
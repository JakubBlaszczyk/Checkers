package com.pk.logic.exceptions;

public class IllegalConstructorException extends Exception {
    public IllegalConstructorException() {
    }

    public IllegalConstructorException(String errorMessage) {
        super(errorMessage);
    }

    public IllegalConstructorException(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}
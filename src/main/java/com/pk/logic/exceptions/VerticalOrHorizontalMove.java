package com.pk.logic.exceptions;

public class VerticalOrHorizontalMove extends Exception {
    public VerticalOrHorizontalMove(String errorMessage) {
        super(errorMessage);
    }

    public VerticalOrHorizontalMove(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}

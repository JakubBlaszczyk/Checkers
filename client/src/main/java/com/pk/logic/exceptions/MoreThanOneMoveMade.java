package com.pk.logic.exceptions;

public class MoreThanOneMoveMade extends Exception {
    public MoreThanOneMoveMade() {
    }

    public MoreThanOneMoveMade(String errorMessage) {
        super(errorMessage);
    }

    public MoreThanOneMoveMade(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}

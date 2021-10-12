package com.pk.logic.exceptions;

public class MandatoryKillMove extends Exception {
    public MandatoryKillMove(String errorMessage) {
        super(errorMessage);
    }

    public MandatoryKillMove(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}

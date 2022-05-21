package com.pk.logic.exceptions;

public class InvalidDirection extends RuntimeException {
  public InvalidDirection() {}

  public InvalidDirection(String errorMessage) {
    super(errorMessage);
  }

  public InvalidDirection(String errorMessage, Throwable err) {
    super(errorMessage, err);
  }
}

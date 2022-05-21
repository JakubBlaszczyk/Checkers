package com.pk.logic.exceptions;

public class IndicesNotFound extends RuntimeException {
  public IndicesNotFound() {}

  public IndicesNotFound(String errorMessage) {
    super(errorMessage);
  }

  public IndicesNotFound(String errorMessage, Throwable err) {
    super(errorMessage, err);
  }
}

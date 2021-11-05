package com.pk.server.exceptions;

/**
 * Checked exception thrown to indicate that error occurred while transferring new Move to Player
 */
public class MoveRejected extends Exception {
  public MoveRejected(String str) {
    super(str);
  }
}

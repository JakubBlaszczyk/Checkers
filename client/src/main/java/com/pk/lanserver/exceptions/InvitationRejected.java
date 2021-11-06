package com.pk.lanserver.exceptions;

/** Checked exception thrown to indicate that invitation was rejected by remote side. */
public class InvitationRejected extends Exception {
  public InvitationRejected(String str) {
    super(str);
  }
}

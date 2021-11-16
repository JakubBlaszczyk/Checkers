package com.pk.models;

import lombok.Value;

@Value
public class Player {
  String nickname;
  String profileImg;

  public String toSendable() {
    return this.nickname + " " + this.profileImg;
  }
}

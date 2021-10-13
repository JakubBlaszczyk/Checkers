package com.pk.server.models;

import java.net.InetAddress;

import lombok.Value;

@Value
public class Player {
  InetAddress ip;
  String nick;
  String profileImg;
}

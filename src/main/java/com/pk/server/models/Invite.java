package com.pk.server.models;

import java.net.InetAddress;
import java.nio.channels.SocketChannel;

import lombok.Value;

@Value
public class Invite {
  InetAddress ip;
  String nick;
  String profileImg;
  SocketChannel sc;
}

package com.pk.models;

import java.nio.channels.SocketChannel;

import lombok.Value;

@Value
public class Session {
  SocketChannel remote;
}

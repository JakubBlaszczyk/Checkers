package com.pk.lanserver.models;

import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;

import lombok.Value;

@Value
public class Connection {
  SelectionKey key;
  SocketChannel sc;
}

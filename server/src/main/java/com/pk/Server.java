package com.pk;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.security.InvalidAlgorithmParameterException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ThreadLocalRandom;

import com.pk.models.Config;
import com.pk.models.Player;

import org.apache.commons.collections4.BidiMap;
import org.apache.commons.collections4.bidimap.DualHashBidiMap;

import lombok.extern.slf4j.Slf4j;

// FIXME detect already closed players, another thread (???)
// FIXME standarize messages, it's mess atm
@Slf4j
public class Server implements Callable<Integer> {
  private Selector selector;
  private ServerSocketChannel serverSocketChannel;
  private Map<SocketChannel, SocketChannel> playersInGames;
  private BidiMap<String, SocketChannel> connectedPlayersMap;
  private BidiMap<SelectionKey, SocketChannel> tmpMap;
  private Set<Player> hashPlayers;

  public Server(String ip, Integer port, Map<SocketChannel, SocketChannel> playersInGames,
      BidiMap<String, SocketChannel> connectedPlayersMap, Set<Player> hashPlayers) throws IOException {
    selector = Selector.open();
    serverSocketChannel = ServerSocketChannel.open();
    serverSocketChannel.configureBlocking(false);
    serverSocketChannel.bind(new InetSocketAddress(ip, port));
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
    this.playersInGames = playersInGames;
    this.connectedPlayersMap = connectedPlayersMap;
    this.hashPlayers = hashPlayers;
    tmpMap = new DualHashBidiMap<>();
  }

  public void cleanup() throws IOException {
    serverSocketChannel.close();
    selector.close();
  }

  @Override
  public Integer call() throws Exception {
    SelectionKey key = null;
    while (true) {
      if (Thread.currentThread().isInterrupted()) {
        return 0;
      }
      if (selector.select() <= 0)
        continue;
      Set<SelectionKey> set = selector.selectedKeys();
      Iterator<SelectionKey> iterator = set.iterator();
      while (iterator.hasNext()) {
        key = iterator.next();
        iterator.remove();
        if (key.isAcceptable()) {
          try {
            handleAccept();
          } catch (IOException e) {
            log.error("Accept error, terminating");
            key.cancel();
            continue;
          }
        }
        if (key.isReadable()) {
          try {
            handleReadMsg(key);
          } catch (IOException e) {
            log.error("Connection error, terminating");
            key.cancel();
            continue;
          }
        }
      }
    }
  }

  private void handleAccept() throws IOException {
    SocketChannel sc = serverSocketChannel.accept();
    sc.configureBlocking(false);
    SelectionKey tmp = sc.register(selector, SelectionKey.OP_READ);
    tmpMap.put(tmp, sc);
    log.info("Connection Accepted: " + sc.getLocalAddress());
  }

  private void handleReadMsg(SelectionKey key) throws IOException {
    SocketChannel sc = (SocketChannel) key.channel();
    ByteBuffer bb = ByteBuffer.allocate(1024);
    int len = sc.read(bb);
    if (len <= 0) {
      sc.close();
      log.info("Connection closed");
      return;
    }
    String msg = new String(bb.array(), 0, len).strip();
    log.info("Message received: " + msg + " \nMessage length: " + msg.length());
    if (msg.startsWith("checkers:config ")) {
      try {
        handleConfig(sc, msg);
      } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    } else if (msg.equals("checkers:randomGame")) {
      try {
        handleRandomGame(key, sc);
      } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    } else {
      log.warn("Got invalid message");
    }
  }

  private void handleConfig(SocketChannel sc, String msg) throws IOException {
    if (connectedPlayersMap.containsValue(sc)) {
      sc.write(ByteBuffer.wrap("checkers:error user already configured".getBytes()));
    }
    Optional<Config> conf = parseConfigMessage(msg.substring(16));
    if (conf.isEmpty()) {
      log.error("Got invalid config message, ignoring");
      return;
    }
    if (connectedPlayersMap.containsKey(conf.get().getNickname())) {
      log.warn("User already present on server: " + conf.get().getNickname());
      sc.write(ByteBuffer.wrap("checkers:error user already present".getBytes()));
      return;
    }
    connectedPlayersMap.put(conf.get().getNickname(), sc);
    hashPlayers.add(new Player(conf.get().getNickname(), conf.get().getProfileImg()));
    ByteBuffer src = ByteBuffer.wrap(transformPlayersListIntoBytes());
    sc.write(src);
  }

  private void handleRandomGame(SelectionKey key, SocketChannel sc) throws IOException {
    if (!connectedPlayersMap.containsValue(sc)) {
      sc.write(ByteBuffer.wrap("checkers:error user not configured".getBytes()));
    }
    key.cancel();
    Optional<List<Socket>> sockets = findPlayersToRandomGame(sc);
    if (sockets.isEmpty()) {
      sc.write(ByteBuffer.wrap("checkers:no players are active :(".getBytes()));
      return;
    }
    List<Socket> tmp = sockets.get();
    Socket sOne = tmp.get(0);
    Socket sTwo = tmp.get(1);
    Thread th = new Thread(new SessionHandler(connectedPlayersMap.getKey(sOne), connectedPlayersMap.getKey(sTwo), sOne,
        sTwo, connectedPlayersMap));
    th.start();
    sOne.getOutputStream().write("checkers:randomStart".getBytes());
    sTwo.getOutputStream().write("checkers:randomStart".getBytes());
  }

  // private void handleInvite() {
  // ;
  // }

  private Optional<List<Socket>> findPlayersToRandomGame(SocketChannel src) {
    if (connectedPlayersMap.size() == 1) {
      return Optional.empty();
    }
    SocketChannel dst = getRandomPlayerChannel(src);
    SelectionKey key = tmpMap.getKey(dst);
    key.cancel();
    try {
      src.configureBlocking(true);
      dst.configureBlocking(true);
    } catch (IOException e) {
      log.error("configureBlocking throw IOException, socketChannel is not canceled (?)", e);
      return Optional.empty();
    }
    return Optional.of(List.of(src.socket(), dst.socket()));
  }
  
  // FIXME prevent calling to game already playing guy
  private SocketChannel getRandomPlayerChannel(SocketChannel src) {
    SocketChannel dst = null;
    int idx = 0;
    boolean flag = false;
    List<Integer> blacklist = new ArrayList<>();
    for (;;) {
      if (flag) {
        break;
      }
      idx = 0;
      int random = ThreadLocalRandom.current().nextInt(0, connectedPlayersMap.size());
      if (blacklist.contains(random)) {
        continue;
      }
      for (SocketChannel sc : connectedPlayersMap.values()) {
        if (idx == random) {
          if (sc == src) {
            if (random == connectedPlayersMap.size() - 1) {
              blacklist.add(random);
              break;
            } else {
              random++;
            }
            idx++;
            continue;
          }
          dst = sc;
          flag = true;
          break;
        }
        idx++;
      }
    }
    return dst;
  }

  private byte[] transformPlayersListIntoBytes() {
    StringBuilder sb = new StringBuilder();
    sb.append("checkers:onlinePlayers ");
    for (Player player : hashPlayers) {
      sb.append(player.toSendable());
      sb.append(" ");
    }
    return sb.toString().strip().getBytes();
  }

  private Optional<Config> parseConfigMessage(String msg) {
    try {
      String[] parts = msg.split(" ");
      // FIXME magic number
      if (parts.length != 2) {
        throw new InvalidAlgorithmParameterException(
            String.format("Invalid len, should be: {}, is {}", 2, parts.length));
      }
      // FIXME add b64 handling
      return Optional.of(new Config(parts[0], parts[1]));
    } catch (Exception e) {
      return Optional.empty();
    }
  }
}

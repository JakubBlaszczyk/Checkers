package com.pk.logic;

import java.util.List;
import java.util.ArrayList;

public class BasicLogic implements Logic {
  public BasicLogic(List<List<Integer>> board) {
    this.board = new ArrayList<>(board.size());
    for (int i = 0; i < board.size(); ++i) {
      this.board.add(new ArrayList<>(board.get(i)));
      }
  }

  public void update(List<List<Integer>> board) {
    throw new UnsupportedOperationException();
  }

  public String toString() {
    StringBuilder result = new StringBuilder();
    for (int i = 0; i < this.board.size(); ++i) {
      for (int j = 0; j < this.board.get(i).size(); ++j) {
        result.append(this.board.get(i).get(j));
      }
      result.append("\n");
    }
    return result.toString();
  }

  private ArrayList<ArrayList<Integer>> board;
}

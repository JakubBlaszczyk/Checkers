package com.pk.logic;

import java.util.List;
import java.util.ArrayList;

import com.pk.logic.exceptions.MandatoryKillMove;
import com.pk.logic.exceptions.MoreThanOneMoveMade;
import com.pk.logic.exceptions.MoveOnAlreadyTakenSpace;
import com.pk.logic.exceptions.VerticalOrHorizontalMove;


public class BasicLogic implements Logic {
  public BasicLogic(List<List<Piece>> board) {
    // fill the board
    this.board = new ArrayList<>(board.size());
    for (int i = 0; i < board.size(); ++i) {
      this.board.add(new ArrayList<>(board.get(i)));
    }
  }

  public void update(List<List<Piece>> board) throws MandatoryKillMove, VerticalOrHorizontalMove, MoreThanOneMoveMade, MoveOnAlreadyTakenSpace {
  }

  public String toString() {
    StringBuilder result = new StringBuilder();
    for (int i = 0; i < this.board.size(); ++i) {
      for (int j = 0; j < this.board.get(i).size(); ++j) {
        switch (this.board.get(i).get(j)) {
          case EMPTY:
          result.append("0");
          break;
          case WHITE_KING:
          result.append("I");
          break;
          case WHITE_PAWN:
          result.append("W");
          break;
          case BLACK_PAWN:
          result.append("B");
          break;
          case BLACK_KING:
          result.append("X");
          break;
        }
      }
      result.append("\n");
    }
    return result.toString();
  }

  private ArrayList<ArrayList<Piece>> board;
}
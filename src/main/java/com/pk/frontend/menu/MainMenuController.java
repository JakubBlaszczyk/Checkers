package com.pk.frontend.menu;


import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;

import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Used to handle GUI operations
 */
public class MainMenuController {

    Locale locale;
    ResourceBundle bundle;

    @FXML
    private Menu game;
    @FXML
    private MenuItem returnToMenu;
    @FXML
    private MenuItem leaveGame;
    @FXML
    private Menu help;
    @FXML
    private MenuItem rules;
    @FXML
    private MenuItem creators;
    @FXML
    private Menu language;
    @FXML
    private MenuItem polish;
    @FXML
    private MenuItem english;
    @FXML
    private Button hotseat;
    @FXML
    private Button multiplayer;
    @FXML
    private Button exit;


    public void switchLanguageToEnglish(){
        setLanguage("en_US");
    }

    public void switchLanguageToPolish(){
        setLanguage("pl_PL");
    }

    private void setLanguage(String lang){
        locale = new Locale(lang);
        bundle = ResourceBundle.getBundle("translations", locale);
        game.setText(bundle.getString("game"));
        returnToMenu.setText(bundle.getString("returnToMenu"));
        leaveGame.setText(bundle.getString("leaveGame"));
        help.setText(bundle.getString("help"));
        rules.setText(bundle.getString("rules"));
        creators.setText(bundle.getString("creators"));
        language.setText(bundle.getString("language"));
        polish.setText(bundle.getString("polish"));
        english.setText(bundle.getString("english"));
        hotseat.setText(bundle.getString("hotseat"));
        multiplayer.setText(bundle.getString("multiplayer"));
        exit.setText(bundle.getString("exit"));
    }
}

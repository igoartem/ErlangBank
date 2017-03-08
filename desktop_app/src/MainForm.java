import javax.swing.*;

public class MainForm extends JFrame {
    private JPanel rootPanel;
    private JButton currencyUpdateButton;
    private JRadioButton getBalanceRadioButton;
    private JRadioButton getMoneyRadioButton;
    private JRadioButton putMoneyRadioButton;
    private JRadioButton giveMoneyRadioButton;
    private JLabel currencyLabel;
    private JLabel depositLabel;
    private JLabel pinLabel;
    private JLabel sumLabel;
    private JLabel depositToLabel;
    private JTextField depositTextField;
    private JPasswordField pinPasswordField;
    private JTextField sumTextField;
    private JTextField depositToTextField;
    private JButton performButton;
    private JTextArea logTextArea;
    private JButton clearLogButton;
    private JRadioButton sendMoneyRadioButton;
    private JLabel sumToLabel;
    private JTextField sumToTextField;
    private JRadioButton getLimitCredit;

    private final Communicator communicator;

    MainForm(Communicator communicator) {
        final String CONNECT_TO_SERVER_ERROR_MESSAGE = "Ошибка соединения с сервером";

        this.communicator = communicator;
        if (communicator.connectToServer()) {
            updateCurrency();
        } else {
            System.err.println(CONNECT_TO_SERVER_ERROR_MESSAGE);
        }

        setContentPane(rootPanel);
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        ButtonGroup radioButtons = new ButtonGroup();

        radioButtons.add(getBalanceRadioButton);
        radioButtons.add(getMoneyRadioButton);
        radioButtons.add(putMoneyRadioButton);
        radioButtons.add(giveMoneyRadioButton);
        radioButtons.add(getLimitCredit);

        getBalanceRadioButton.addActionListener(e -> {
            setSumEnabled(false);
            setDepositToEnabled(false);
        });

        getMoneyRadioButton.addActionListener(e -> {
            setSumEnabled(true);
            setDepositToEnabled(false);
        });

        putMoneyRadioButton.addActionListener(e -> {
            setSumEnabled(true);
            setDepositToEnabled(false);
        });

        giveMoneyRadioButton.addActionListener(e -> {
            setDepositToEnabled(true);
        });

        getLimitCredit.addActionListener(e -> {

        });

        currencyUpdateButton.addActionListener(e -> {
            if (communicator.connectToServer()) {
                updateCurrency();
            } else {
                System.err.println(CONNECT_TO_SERVER_ERROR_MESSAGE);
            }
        });

        performButton.addActionListener(e -> {
            if (communicator.connectToServer()) {
                int deposit = Integer.valueOf(depositTextField.getText());
                int pin = Integer.valueOf(new String(pinPasswordField.getPassword()));
                if (getBalanceRadioButton.isSelected()) {
                    Communicator.Balance balance = communicator.getBalance(deposit, pin);
                    if (balance != null) {
                        logTextArea.append("Баланс карты " + deposit + ": " + balance + "\n");
                    } else {
                        logTextArea.append("Некорректный пин\n");
                    }
                } else {
                    double sum = Double.valueOf(sumTextField.getText());
                    if (getMoneyRadioButton.isSelected()) {
                        if (communicator.getMoney(deposit, pin, sum)) {
                            logTextArea.append("Средства успешно списаны\n");
                        } else {
                            logTextArea.append("Ошибка списания средств\n");
                        }
                    } else if (putMoneyRadioButton.isSelected()) {
                        if (communicator.putMoney(deposit, pin, sum)) {
                            logTextArea.append("Средства успешно внесены\n");
                        } else {
                            logTextArea.append("Ошибка внесения средств\n");
                        }
                    } else if (sendMoneyRadioButton.isSelected()) {
                        int depositTo = Integer.valueOf(depositToTextField.getText());
                        if (communicator.sendMoney(deposit, pin, depositTo, sum)) {
                            logTextArea.append("Средства успешно переведены\n");
                        } else {
                            logTextArea.append("Ошибка перевода средств\n");
                        }
                    } else if (giveMoneyRadioButton.isSelected()) {
                        int sumTo = Integer.valueOf(sumToTextField.getText());
                        if (communicator.giveMoney(deposit, pin, sumTo)) {
                            logTextArea.append("Кредит успешно получен\n");
                        } else {
                            logTextArea.append("Превышен лимит кредита\n");
                        }
                    } else {
                        int d = communicator.getLimitCredit();
                        if (d==-1) {
                            logTextArea.append("Ошибка получения лимита\n");
                        } else {
                            logTextArea.append("Лимит: " + d + "\n");
                        }
                    }
                }
            } else {
                System.err.println(CONNECT_TO_SERVER_ERROR_MESSAGE);
            }
        });

        clearLogButton.addActionListener(e -> {
            logTextArea.setText("");
        });
    }

    private void setSumEnabled(boolean value) {
        sumLabel.setEnabled(value);
        sumTextField.setEnabled(value);
    }

    private void setDepositToEnabled(boolean value) {
        depositToLabel.setEnabled(value);
        depositToTextField.setEnabled(value);
    }

    private void updateCurrency() {
        int[] currency = communicator.getCurrency();
        int eur = currency[0];
        int usd = currency[1];
        currencyLabel.setText("EUR: " + eur + ", USD: " + usd);
    }
}

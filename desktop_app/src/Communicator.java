import com.ericsson.otp.erlang.*;

import java.io.IOException;

class Communicator {

    private static final String BAD_RPC = "badrpc";

    private String serverNodeName;
    private String selfNodeName;

    private boolean connected;

    private OtpConnection connection;

    public static class Balance {
        double value;
        String currency;

        Balance(double value, String currency) {
            this.value = value;
            this.currency = currency;
        }

        @Override
        public String toString() {
            return value + " " + currency;
        }
    }

    Communicator(String serverNodeName, String selfNodeName) {
        this.serverNodeName = serverNodeName;
        this.selfNodeName = selfNodeName;
        this.connected = false;
    }

    boolean connectToServer() {
        if (!connected) {
            OtpSelf self;
            OtpPeer server;
            try {
                self = new OtpSelf(selfNodeName);
                server = new OtpPeer(serverNodeName);
            } catch (IOException e) {
                e.printStackTrace();
                return false;
            }
            self.setCookie(server.cookie());
            try {
                connection = self.connect(server);
            } catch (Exception e) {
                e.printStackTrace();
                return false;
            }
            connected = true;
        }
        return true;
    }

    int[] getCurrency() {
        int[] res = new int[2];
        res[0] = -1;

        try {
            connection.sendRPC("bank_server", "getCurrency", new OtpErlangList());
        } catch (IOException e) {
            e.printStackTrace();
            return res;
        }

        OtpErlangTuple received;
        try {
            received = (OtpErlangTuple) connection.receiveRPC();
        } catch (Exception e) {
            e.printStackTrace();
            return res;
        }

        if (!received.toString().contains(BAD_RPC)) {
            res[0] = Integer.valueOf(received.elementAt(0).toString());
            res[1] = Integer.valueOf(received.elementAt(1).toString());
        } else {
            System.err.println(received);
        }

        return res;
    }

    Balance getBalance(int deposit, int pin) {
        Balance balance = null;

        try {
            connection.sendRPC("bank_server", "getBalance", new OtpErlangList(new OtpErlangObject[] {
                    new OtpErlangInt(deposit), new OtpErlangInt(pin)
            }));
        } catch (IOException e) {
            e.printStackTrace();
        }

        OtpErlangObject received;
        try {
            received = connection.receiveRPC();
        } catch (Exception e) {
            e.printStackTrace();
            return new Balance(-1, "usd");
        }

        if (!received.toString().contains(BAD_RPC)) {
            if (!received.toString().equals("false")) {
                double value = Double.valueOf(((OtpErlangTuple) received).elementAt(0).toString());
                String currency = ((OtpErlangTuple) received).elementAt(1).toString();
                balance = new Balance(value, currency);
            } else {
                System.err.println("false");
            }
        } else {
            System.err.println(received);
        }

        return balance;
    }

    boolean putMoney(int deposit, int pin, double sum) {
        try {
            connection.sendRPC("bank_server", "putMoney", new OtpErlangList(new OtpErlangObject[] {
                    new OtpErlangInt(deposit), new OtpErlangInt(pin), new OtpErlangDouble(sum)
            }));
        } catch (IOException e) {
            e.printStackTrace();
        }

        OtpErlangObject received;
        try {
            received = connection.receiveRPC();
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }

        if (received.toString().contains(BAD_RPC)) {
            System.err.println(received);
            return false;
        }

        return Boolean.valueOf(received.toString());
    }

    boolean getMoney(int deposit, int pin, double sum) {
        try {
            connection.sendRPC("bank_server", "getMoney", new OtpErlangList(new OtpErlangObject[] {
                    new OtpErlangInt(deposit), new OtpErlangInt(pin), new OtpErlangDouble(sum)
            }));
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }

        OtpErlangObject received;
        try {
            received = connection.receiveRPC();
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }

        if (received.toString().contains(BAD_RPC)) {
            System.err.println(received);
            return false;
        }

        return !received.toString().equals("false");
    }

    boolean sendMoney(int deposit, int pin, int depositTo, double sum) {
        try {
            connection.sendRPC("bank_server", "sendMoney", new OtpErlangList(new OtpErlangObject[] {
                    new OtpErlangInt(deposit), new OtpErlangInt(pin), new OtpErlangInt(depositTo), new OtpErlangDouble(sum)
            }));
        } catch (IOException e) {
            e.printStackTrace();
        }

        OtpErlangObject received;
        try {
            received = connection.receiveRPC();
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }

        if (received.toString().contains(BAD_RPC)) {
            System.err.println(received);
            return false;
        }

        return Boolean.valueOf(received.toString());
    }

    boolean giveMoney(int deposit, int pin, int sumTo) {
        try {
            connection.sendRPC("bank_server", "giveMoney", new OtpErlangList(new OtpErlangObject[] {
                    new OtpErlangInt(deposit),new OtpErlangInt(pin), new OtpErlangInt(sumTo)
            }));
        } catch (IOException e) {
            e.printStackTrace();
        }

        OtpErlangObject received;
        try {
            received = connection.receiveRPC();
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }

        if (received.toString().contains(BAD_RPC)) {
            System.err.println(received);
            return false;
        }

        return Boolean.valueOf(received.toString());
    }

    int getLimitCredit(){
        try {
            connection.sendRPC("bank_server", "getLimitMoney",new OtpErlangList());
        } catch (IOException e) {
            e.printStackTrace();
        }

        OtpErlangObject received;
        try {
            received = connection.receiveRPC();
        } catch (Exception e) {
            e.printStackTrace();
            return -1;
        }
        if (received.toString().contains(BAD_RPC)) {
            System.err.println(received);
            return -1;
        }
        return Integer.parseInt(received.toString());
    }
}

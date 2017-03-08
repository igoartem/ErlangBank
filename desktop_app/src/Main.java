public class Main {
    public static void main(String[] args) {
        String serverNodeName = args[0];
        String selfNodeName = args[1];
        Communicator communicator = new Communicator(serverNodeName, selfNodeName);
        MainForm form = new MainForm(communicator);
        form.setTitle("Алтайка банк");
        form.setVisible(true);
    }
}

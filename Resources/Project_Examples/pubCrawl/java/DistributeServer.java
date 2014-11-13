import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

public class DistributeServer implements Compute {

    public DistributeServer(){
        super();
    }

    public PCObject callFunction(IPCFunction function, PCObject param){
        return function.call(param);
    }

    public static void main(String[] args){
        System.setProperty("java.security.policy", "server.policy");
        System.setProperty("java.rmi.server.useCodebaseOnly","false");
        if (System.getSecurityManager() == null) {
            System.setSecurityManager(new SecurityManager());
        }

        try {
            String name = "Compute";

            Compute engine = new DistributeServer();
            Compute stub =
                    (Compute) UnicastRemoteObject.exportObject(engine, 0);

            Registry registry = LocateRegistry.createRegistry(Integer.parseInt(args[0]));

            registry.rebind(name, stub);

            System.out.println("Server started");
        } catch (Exception e) {
            System.err.println("Server exception:");
            e.printStackTrace();
            System.exit(1);
        }
    }
}



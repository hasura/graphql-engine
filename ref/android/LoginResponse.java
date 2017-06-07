import com.google.gson.annotations.SerializedName;
import java.util.List;

public class LoginResponse {

    @SerializedName("auth_token")
    String authToken;

    @SerializedName("hasura_id")
    Integer id;

    @SerializedName("hasura_roles")
    List<String> roles;

    public String getAuthToken() {
        return authToken;
    }

    public Integer getId() {
        return id;
    }

    public List<String> getRoles() {
        return roles;
    }
}


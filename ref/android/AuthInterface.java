import com.example.jaison.hasura_todo_android.models.AuthRequest;
import com.example.jaison.hasura_todo_android.models.AuthResponse;
import com.example.jaison.hasura_todo_android.models.MessageResponse;

import retrofit2.Call;
import retrofit2.http.Body;
import retrofit2.http.POST;

public interface HasuraAuthInterface {

    @POST("login")
    Call<AuthResponse> login(@Body AuthRequest request);
}

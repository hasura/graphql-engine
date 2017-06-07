import com.google.gson.annotations.SerializedName;

public class ArticleRecord {

    @SerializedName("id")
    Integer id;

    @SerializedName("name")
    String name;

    @SerializedName("user_id")
    Integer userId;
}

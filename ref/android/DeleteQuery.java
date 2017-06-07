import com.google.gson.annotations.SerializedName;
import java.util.List;

public class DeleteTodoQuery {

    @SerializedName("type")
    String type = "delete";

    @SerializedName("args")
    Args args;

    public DeleteTodoQuery(Integer articleId, Integer userId) {
        args = new Args();
        args.where = new Where();
        args.where.id = articleId;
        args.where.userId = userId;
    }

    class Args {

        @SerializedName("table")
        String table = "article";

        @SerializedName("where")
        Where where;
    }

    class Where {
        @SerializedName("user_id")
        Integer userId;

        @SerializedName("id")
        Integer id;
    }
}

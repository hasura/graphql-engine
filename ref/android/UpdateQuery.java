import com.google.gson.annotations.SerializedName;

public class UpdateTodoQuery {

    @SerializedName("type")
    String type = "update";

    @SerializedName("args")
    Args args;

    public UpdateTodoQuery(Integer articleId, Integer userId, String name) {
        args = new Args();
        args.where = new Where();
        args.where.id = articleId;
        args.where.userId = userId;
        args.set = new Set();
        args.set.name = name;
    }

    class Args {

        @SerializedName("table")
        String table = "todo";

        @SerializedName("where")
        Where where;

        @SerializedName("$set")
        Set set;
    }

    class Where {
        @SerializedName("user_id")
        Integer userId;

        @SerializedName("id")
        Integer id;
    }

    class Set {
        @SerializedName("name")
        String name;
    }
}


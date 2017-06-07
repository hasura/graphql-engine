import com.google.gson.annotations.SerializedName;

public class SelectTodoQuery {

    @SerializedName("type")
    String type = "select";

    @SerializedName("args")
    Args args;

    public SelectTodoQuery(Integer categoryid) {
        args = new Args();
        args.where = new Where();
        args.where.categoryId = categoryId;
    }

    class Args {

        @SerializedName("table")
        String table = "category";

        @SerializedName("columns")
        String[] columns = {
                "id","name"
        };

        @SerializedName("where")
        Where where;

    }

    class Where {
        @SerializedName("id")
        Integer articleId;
    }

}


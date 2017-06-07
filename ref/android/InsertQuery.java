import com.google.gson.annotations.SerializedName;
import java.util.ArrayList;
import java.util.List;

public class InsertQuery {

    @SerializedName("type")
    String type = "insert";

    @SerializedName("args")
    Args args;

    public InsertTodoQuery(List<ArticleRecord> data) {
        args = new Args();
        args.objects = new ArrayList<>();
        args.objects.add(data);
    }

    class Args {

        @SerializedName("table")
        String table = "category";

        @SerializedName("returning")
        String[] returning = {
                "id","name"
        };

        @SerializedName("objects")
        List<ArticleRecord> objects;

    }
}

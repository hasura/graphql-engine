import com.google.gson.annotations.SerializedName;
import java.util.List;

public class ReturningResponse {

    @SerializedName("affected_rows")
    Integer affectedRows;

    @SerializedName("returning")
    List<ArticleRecord> records;

    public Integer getAffectedRows() {
        return affectedRows;
    }

    public List<ArticleRecord> getArticleRecords() {
        return records;
    }
}

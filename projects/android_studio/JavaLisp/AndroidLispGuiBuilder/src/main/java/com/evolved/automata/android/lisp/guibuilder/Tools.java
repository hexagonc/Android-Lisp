package com.evolved.automata.android.lisp.guibuilder;

import com.dropbox.core.v2.files.Metadata;
import com.evolved.automata.lisp.Environment;
import com.evolved.automata.lisp.FunctionTemplate;
import com.evolved.automata.lisp.NLispTools;
import com.evolved.automata.lisp.SimpleFunctionTemplate;
import com.evolved.automata.lisp.Value;

import org.greenrobot.eventbus.EventBus;
import org.json.JSONObject;

import java.util.HashMap;


/**
 * Created by Evolved8 on 5/11/17.
 */

public class Tools {




    public static final HashMap<String, Integer> DRAWABLE_RESOURCE_MAP = new HashMap<String, Integer>()
    {
        {
            put("ic_keyboard_black_24dp", R.drawable.ic_keyboard_black_24dp);
            put("dropbox_logos_dropbox_glyph_blue", R.drawable.dropbox_logos_dropbox_glyph_blue);
            put("ic_add_a_photo_black_24dp", R.drawable.ic_add_a_photo_black_24dp);
            put("ic_add_alert_black_24dp", R.drawable.ic_add_alert_black_24dp);
            put("ic_add_black_24dp", R.drawable.ic_add_black_24dp);
            put("ic_alarm_add_black_24dp", R.drawable.ic_alarm_add_black_24dp);
            put("ic_alarm_black_24dp", R.drawable.ic_alarm_black_24dp);
            put("ic_alarm_off_black_24dp", R.drawable.ic_alarm_off_black_24dp);
            put("ic_alarm_on_black_24dp", R.drawable.ic_alarm_on_black_24dp);
            put("ic_arrow_back_black_24dp", R.drawable.ic_arrow_back_black_24dp);
            put("ic_arrow_downward_black_24dp", R.drawable.ic_arrow_downward_black_24dp);
            put("ic_arrow_forward_black_24dp", R.drawable.ic_arrow_forward_black_24dp);
            put("ic_arrow_upward_black_24dp", R.drawable.ic_arrow_upward_black_24dp);
            put("ic_attachment_black_24dp", R.drawable.ic_attachment_black_24dp);
            put("ic_audiotrack_black_24dp", R.drawable.ic_audiotrack_black_24dp);
            put("ic_build_black_24dp", R.drawable.ic_build_black_24dp);
            put("ic_call_made_black_24dp", R.drawable.ic_call_made_black_24dp);
            put("ic_call_merge_black_24dp", R.drawable.ic_call_merge_black_24dp);
            put("ic_chevron_left_black_24dp", R.drawable.ic_chevron_left_black_24dp);
            put("ic_chevron_right_black_24dp", R.drawable.ic_chevron_right_black_24dp);
            put("ic_clear_black_24dp", R.drawable.ic_clear_black_24dp);
            put("ic_close_black_24dp", R.drawable.ic_close_black_24dp);
            put("ic_cloud_black_24dp", R.drawable.ic_cloud_black_24dp);
            put("ic_cloud_download_black_24dp", R.drawable.ic_cloud_download_black_24dp);
            put("ic_code_snippet_cut_black_36dp", R.drawable.ic_code_snippet_cut_black_36dp);
            put("ic_compare_arrows_black_24dp", R.drawable.ic_compare_arrows_black_24dp);
            put("ic_content_copy_black_24dp", R.drawable.ic_content_copy_black_24dp);
            put("ic_content_cut_black_24dp", R.drawable.ic_content_cut_black_24dp);
            put("ic_content_cut_black_36dp", R.drawable.ic_content_cut_black_36dp);
            put("ic_content_paste_black_24dp", R.drawable.ic_content_paste_black_24dp);
            put("ic_create_new_folder_black_24dp", R.drawable.ic_create_new_folder_black_24dp);
            put("ic_date_range_black_24dp", R.drawable.ic_date_range_black_24dp);
            put("ic_delete_black_24dp", R.drawable.ic_delete_black_24dp);
            put("ic_description_black_24dp", R.drawable.ic_description_black_24dp);
            put("ic_done_black_24dp", R.drawable.ic_done_black_24dp);
            put("ic_error_black_24dp", R.drawable.ic_error_black_24dp);
            put("ic_error_outline_black_24dp", R.drawable.ic_error_outline_black_24dp);
            put("ic_expand_less_black_24dp", R.drawable.ic_expand_less_black_24dp);
            put("ic_expand_more_black_24dp", R.drawable.ic_expand_more_black_24dp);
            put("ic_file_download_black_24dp", R.drawable.ic_file_download_black_24dp);
            put("ic_file_upload_black_24dp", R.drawable.ic_file_upload_black_24dp);
            put("ic_folder_black_24dp", R.drawable.ic_folder_black_24dp);
            put("ic_folder_open_black_24dp", R.drawable.ic_folder_open_black_24dp);
            put("ic_format_align_left_black_24dp", R.drawable.ic_format_align_left_black_24dp);
            put("ic_format_list_bulleted_black_24dp", R.drawable.ic_format_list_bulleted_black_24dp);
            put("ic_fullscreen_black_24dp", R.drawable.ic_fullscreen_black_24dp);
            put("ic_gps_fixed_black_24dp", R.drawable.ic_gps_fixed_black_24dp);
            put("ic_home_black_24dp", R.drawable.ic_home_black_24dp);
            put("ic_hourglass_empty_black_24dp", R.drawable.ic_hourglass_empty_black_24dp);
            put("ic_hourglass_full_black_24dp", R.drawable.ic_hourglass_full_black_24dp);
            put("ic_image_black_24dp", R.drawable.ic_image_black_24dp);
            put("ic_insert_comment_black_24dp", R.drawable.ic_insert_comment_black_24dp);
            put("ic_insert_link_black_24dp", R.drawable.ic_insert_link_black_24dp);
            put("ic_launch_black_24dp", R.drawable.ic_launch_black_24dp);
            put("ic_launcher", R.drawable.ic_launcher);
            put("ic_lightbulb_outline_black_24dp", R.drawable.ic_lightbulb_outline_black_24dp);
            put("ic_location_on_black_24dp", R.drawable.ic_location_on_black_24dp);
            put("ic_lock_black_24dp", R.drawable.ic_lock_black_24dp);
            put("ic_lock_open_black_24dp", R.drawable.ic_lock_open_black_24dp);
            put("ic_mic_black_24dp", R.drawable.ic_mic_black_24dp);
            put("ic_mic_off_black_24dp", R.drawable.ic_mic_off_black_24dp);
            put("ic_my_location_black_24dp", R.drawable.ic_my_location_black_24dp);
            put("ic_navigate_before_black_24dp", R.drawable.ic_navigate_before_black_24dp);
            put("ic_navigate_next_black_24dp", R.drawable.ic_navigate_next_black_24dp);
            put("ic_not_interested_black_24dp", R.drawable.ic_not_interested_black_24dp);
            put("ic_note_add_black_24dp", R.drawable.ic_note_add_black_24dp);
            put("ic_note_black_24dp", R.drawable.ic_note_black_24dp);
            put("ic_open_in_browser_black_24dp", R.drawable.ic_open_in_browser_black_24dp);
            put("ic_open_in_new_black_24dp", R.drawable.ic_open_in_new_black_24dp);
            put("ic_pan_tool_black_24dp", R.drawable.ic_pan_tool_black_24dp);
            put("ic_person_black_24dp", R.drawable.ic_person_black_24dp);
            put("ic_photo_camera_black_24dp", R.drawable.ic_photo_camera_black_24dp);
            put("ic_place_black_24dp", R.drawable.ic_place_black_24dp);
            put("ic_play_arrow_black_24dp", R.drawable.ic_play_arrow_black_24dp);
            put("ic_priority_high_black_24dp", R.drawable.ic_priority_high_black_24dp);
            put("ic_refresh_black_24dp", R.drawable.ic_refresh_black_24dp);
            put("ic_save_black_24dp", R.drawable.ic_save_black_24dp);
            put("ic_sd_storage_black_24dp", R.drawable.ic_sd_storage_black_24dp);
            put("ic_sd_storage_black_36dp", R.drawable.ic_sd_storage_black_36dp);
            put("ic_sd_storage_black_48dp", R.drawable.ic_sd_storage_black_48dp);
            put("ic_search_black_24dp", R.drawable.ic_search_black_24dp);
            put("ic_select_all_black_24dp", R.drawable.ic_select_all_black_24dp);
            put("ic_settings_black_24dp", R.drawable.ic_settings_black_24dp);
            put("ic_share_black_24dp", R.drawable.ic_share_black_24dp);
            put("ic_skip_next_black_24dp", R.drawable.ic_skip_next_black_24dp);
            put("ic_skip_previous_black_24dp", R.drawable.ic_skip_previous_black_24dp);
            put("ic_sort_black_24dp", R.drawable.ic_sort_black_24dp);
            put("ic_speaker_notes_black_24dp", R.drawable.ic_speaker_notes_black_24dp);
            put("ic_speaker_notes_off_black_24dp", R.drawable.ic_speaker_notes_off_black_24dp);
            put("ic_spellcheck_black_24dp", R.drawable.ic_spellcheck_black_24dp);
            put("ic_stop_black_24dp", R.drawable.ic_stop_black_24dp);
            put("ic_swap_horiz_black_24dp", R.drawable.ic_swap_horiz_black_24dp);
            put("ic_swap_vert_black_24dp", R.drawable.ic_swap_vert_black_24dp);
            put("ic_sync_black_24dp", R.drawable.ic_sync_black_24dp);
            put("ic_sync_problem_black_24dp", R.drawable.ic_sync_problem_black_24dp);
            put("ic_thumb_down_black_24dp", R.drawable.ic_thumb_down_black_24dp);
            put("ic_thumb_up_black_24dp", R.drawable.ic_thumb_up_black_24dp);
            put("ic_thumbs_up_down_black_24dp", R.drawable.ic_thumbs_up_down_black_24dp);
            put("ic_timer_black_24dp", R.drawable.ic_timer_black_24dp);
            put("ic_timer_off_black_24dp", R.drawable.ic_timer_off_black_24dp);
            put("ic_title_black_24dp", R.drawable.ic_title_black_24dp);
            put("ic_today_black_24dp", R.drawable.ic_today_black_24dp);
            put("ic_trending_down_black_24dp", R.drawable.ic_trending_down_black_24dp);
            put("ic_trending_flat_black_24dp", R.drawable.ic_trending_flat_black_24dp);
            put("ic_trending_up_black_24dp", R.drawable.ic_trending_up_black_24dp);
            put("ic_undo_black_24dp", R.drawable.ic_undo_black_24dp);
            put("ic_view_list_black_24dp", R.drawable.ic_view_list_black_24dp);
            put("ic_visibility_black_24dp", R.drawable.ic_visibility_black_24dp);
            put("ic_volume_mute_black_24dp", R.drawable.ic_volume_mute_black_24dp);
            put("ic_volume_off_black_24dp", R.drawable.ic_volume_off_black_24dp);
            put("ic_warning_black_24dp", R.drawable.ic_warning_black_24dp);
            put("ic_web_black_24dp", R.drawable.ic_web_black_24dp);
        }
    };

    public static Environment addAndroidToolFunctions(Environment env)
    {
        env.mapFunction("get-drawable-resource-id", getDrawableResourceId());
        return env;
    }

    public static SimpleFunctionTemplate getDrawableResourceId()
    {
        return new SimpleFunctionTemplate()
        {
            @SuppressWarnings("unchecked")
            @Override
            public <T extends FunctionTemplate> T innerClone() throws java.lang.InstantiationException, IllegalAccessException
            {
                return (T)getDrawableResourceId();
            }

            @Override
            public Value evaluate(Environment env, Value[] evaluatedArgs) {
                checkActualArguments(1, true, true);
                String resourceName = evaluatedArgs[0].getString();

                Integer resourceId = DRAWABLE_RESOURCE_MAP.get(resourceName);
                if (resourceId != null)
                    return NLispTools.makeValue(resourceId.intValue());
                else
                    return Environment.getNull();

            }
        };
    }


    static EventBus mMain = EventBus.getDefault();



    public static void postEvent(Object o)
    {
        mMain.post(o);
    }

    public static void postStickyEvent(Object o)
    {
        mMain.postSticky(o);
    }


    public static void registerEventHandler(Object o)
    {

        mMain.register(o);
    }

    public static void unRegisterEventHandler(Object o)
    {
        mMain.unregister(o);
    }



    public static boolean isFolder(Metadata dbxMetaData)
    {
        String sForm = dbxMetaData.toString();
        try
        {
            JSONObject jobject = new JSONObject(sForm);
            String typeName = jobject.getString(".tag");
            return  "folder".equals(typeName);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.toString());
        }

    }

    public static String getParentFolder(String fullFileName)
    {
        StringBuilder parentReady = new StringBuilder("/"), segment = new StringBuilder();

        for (char c:fullFileName.toCharArray())
        {
            if (c == '/')
            {
                if (parentReady.length() > 1)
                    parentReady.append('/');
                parentReady.append(segment);
                segment = new StringBuilder();
            }
            else
                segment.append(c);
        }

        return parentReady.toString();

    }





}

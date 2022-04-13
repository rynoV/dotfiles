function push_onedriveschool --description 'Push local changes to school OneDrive using rclone'
    rclone sync --fast-list -i --filter-from ~/.config/rclone/onedriveschool-filter.txt ~/OneDriveSchool/ onedriveschool:
end

function pull_onedriveschool --description 'Pull changes from school OneDrive using rclone'
    rclone sync --fast-list -i --filter-from ~/.config/rclone/onedriveschool-filter.txt onedriveschool: ~/OneDriveSchool/
end

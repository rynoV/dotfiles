function pull_onedrive --description 'Pull changes from OneDrive using rclone'
    rclone sync --fast-list -i --filter-from ~/.config/rclone/onedrive-filter.txt onedrive: ~/OneDrive/
end

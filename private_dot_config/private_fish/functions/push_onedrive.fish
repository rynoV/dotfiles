function push_onedrive --description 'Push local changes to OneDrive using rclone'
    rclone sync --fast-list -i --filter-from ~/.config/rclone/onedrive-filter.txt ~/OneDrive/ onedrive:
end

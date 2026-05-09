module.exports = {
    apps: [
        {
            name: "ahawiki-10000",
            cwd: "/home/ubuntu/www/wiki.aha00a.com",
            script: "bash",
            args: ["-lc", "rm -f target/universal/stage/RUNNING_PID_10000 && ./start10000.sh"],
            autorestart: true
        },
        {
            name: "ahawiki-10001",
            cwd: "/home/ubuntu/www/wiki.aha00a.com",
            script: "bash",
            args: ["-lc", "rm -f target/universal/stage/RUNNING_PID_10001 && ./start10001.sh"],
            autorestart: true
        }
    ]
}

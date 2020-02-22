package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"sync"

	"github.com/gin-contrib/static"
	"github.com/gin-gonic/gin"
)

var (
	unitsInfo      = make([]unitInfo, 0)
	unitsInfoMutex = sync.RWMutex{}
)

type position []float64

type unitInfo struct {
	NetID       string   `json:"netid"`
	IsPlayer    bool     `json:"isplayer"`
	ProfileName string   `json:"profilename"`
	Side        string   `json:"side"`
	Position    position `json:"position"`
}

func (u *unitInfo) UnmarshalJSON(buf []byte) error {
	tmp := []interface{}{
		&u.NetID,
		&u.IsPlayer,
		&u.ProfileName,
		&u.Side,
		&u.Position,
	}
	wantLen := len(tmp)
	if err := json.Unmarshal(buf, &tmp); err != nil {
		return err
	}
	if g, e := len(tmp), wantLen; g != e {
		return fmt.Errorf("wrong number of fields in Notification: %d != %d", g, e)
	}
	return nil
}

func main() {
	listenAddress := os.Getenv("LISTEN_ADDR")
	if listenAddress == "" {
		listenAddress = ":5000"
	}

	router := gin.Default()
	router.Use(static.Serve("/", static.LocalFile("../ui/public", false)))
	router.Use(static.Serve("/maps", static.LocalFile("../maps", false)))

	router.GET("/ping", func(c *gin.Context) {
		c.String(http.StatusOK, "pong")
	})

	router.POST("/ping", func(c *gin.Context) {
		c.JSON(http.StatusOK, gin.H{
			"message": "pong",
		})
	})

	router.POST("/units-info", updateUnitsInfo)
	router.GET("/units-info", getUnitsInfo)

	router.Run(listenAddress)
}

func updateUnitsInfo(c *gin.Context) {
	ups := make([]unitInfo, 0)
	err := c.ShouldBindJSON(&ups)
	if err != nil {
		log.Println(err)
		c.JSON(http.StatusBadRequest, err)
		return
	}

	unitsInfoMutex.Lock()
	defer unitsInfoMutex.Unlock()
	unitsInfo = ups

	c.JSON(http.StatusOK, nil)
}

func getUnitsInfo(c *gin.Context) {
	unitsInfoMutex.RLock()
	defer unitsInfoMutex.RUnlock()
	c.JSON(http.StatusOK, unitsInfo)
}

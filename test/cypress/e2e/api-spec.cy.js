describe('DiAGRAM API spec', () => {
    it('should get the model score when creating a new model', () => {
        cy.request({
            method: 'POST',
            url: '/api/model/score',
            body: {"model_name":"fdg","scenario":"Base Model","notes":"","is_advanced":false,"intellectual_control":null,"renderability":null,"response":{"Digital_Object":[0,0,100],"Storage_Medium":[0,0,100],"Rep_and_Refresh":{"1":0,"2":0},"Op_Environment":{"1":0,"2":"Not Applicable - we have copies offsite"},"Physical_Disaster":"Very Low","Checksum":[0,0,100],"System_Security":{"1":"ISO 27001","2":"None, or only minor issues outstanding","3":"Level 4","4":"Yes"},"Info_Management":{"1":"Not achieved","2":"Not achieved","3":["Minimal awareness","Minimal awareness"]},"Technical_Skills":["None","None","None","None","None","None","None","None","None","None"]},"advanced":null},
            headers: {
                'Content-type': 'application/json; charset=UTF-8',
            },
        }).then((response) => {
            expect(response.status).to.eq(200);
            expect(response.body).to.include({"renderability": 44.679});
            expect(response.body).to.have.property('nodes');
            expect(response.body.nodes).to.have.property('Op_Environment');
            expect(response.body.nodes.Op_Environment).to.include({Yes: 0, No: 1});
        });
    });

    it('uploading an invalid model should succeeed and report the model to be invalid', () => {
        cy.request({
            method: 'POST',
            url: '/api/validation/validate_json',
            body: {"model_name":"fdg", "random_stuff": "not_valid"},
            headers: {
                'Content-type': 'application/json; charset=UTF-8',
            },
        }).then((response) => {
            expect(response.status).to.eq(200);
            expect(response.body).to.include({"status": false});
        });
    });

    it('uploading a valid model should succeed and report the model to be valid', () => {
        cy.request({
            method: 'POST',
            url: '/api/validation/validate_json',
            body: [{"model_name":"Valid_Model","scenario":"Base Model","notes":"","is_advanced":false,"intellectual_control":0,"renderability":42,"response":{"Digital_Object":[50,0,50],"Storage_Medium":[0,0,100],"Rep_and_Refresh":{"1":80,"2":74},"Op_Environment":{"1":50,"2":"Not Applicable - we have copies offsite"},"Physical_Disaster":"Very Low","Checksum":[50,0,50],"System_Security":{"1":"ISO 27001","2":"None, or only minor issues outstanding","3":"Level 4","4":"Yes"},"Info_Management":{"1":"Not achieved","2":"Not achieved","3":["Minimal awareness","Minimal awareness"]},"Technical_Skills":["None","None","None","None","None","None","None","None","None","None"]},"advanced":{"Op_Environment":{"Yes":0.5,"No":0.5},"Integrity":{"System_Security":["Good","Poor","Good","Poor","Good","Poor","Good","Poor","Good","Poor","Good","Poor"],"Info_Management":["Sufficient","Sufficient","Insufficient","Insufficient","Sufficient","Sufficient","Insufficient","Insufficient","Sufficient","Sufficient","Insufficient","Insufficient"],"Checksum":["Yes","Yes","Yes","Yes","Self_Generated","Self_Generated","Self_Generated","Self_Generated","No","No","No","No"],"Yes":[1,0.5326,0,0,0.9099,0.4424,0,0,0,0,0,0],"No":[0,0.4674,1,1,0.0901,0.5576,1,1,1,1,1,1]},"System_Security":{"Good":1,"Poor":0},"Info_Management":{"Sufficient":0,"Insufficient":1},"Storage_Medium":{"A":0,"B":0,"C":1},"Rep_and_Refresh":{"Good":0.592,"Poor":0.408},"Digital_Object":{"Born_digital":0.5,"Digitised":0,"Surrogate":0.5},"Content_Metadata":{"Digital_Object":["Born_digital","Digitised","Surrogate"],"Yes":[0.4539,0.7492,0.7261],"No":[0.5461,0.2508,0.2739]},"Tech_Metadata":{"Technical_Skills":["Good","Poor"],"Sufficient":[0.7729,0.427],"Insufficient":[0.2271,0.573]},"File_Format":{"Digital_Object":["Born_digital","Digitised","Surrogate"],"Yes":[0.8589,0.9999,0.9999],"No":[0.1411,0.0001,0.0001]},"Checksum":{"Yes":0.5,"Self_Generated":0,"No":0.5},"Obsolescence":{"Storage_Medium":["A","B","C","A","B","C"],"Technical_Skills":["Good","Good","Good","Poor","Poor","Poor"],"Yes":[0.2985,0.1405,0.001,0.6422,0.419,0.001],"No":[0.7015,0.8595,0.999,0.3578,0.581,0.999]},"Tools_to_Render":{"File_Format":["Yes","No","Yes","No"],"Technical_Skills":["Good","Good","Poor","Poor"],"Yes":[0.8111,0.4343,0.8111,0],"No":[0.1889,0.5657,0.1889,1]},"Intellectual_Control":{"Conditions_of_Use":["Yes","No","Yes","No"],"Identity":["Yes","Yes","No","No"],"Yes":[1,0,0,0],"No":[0,1,1,1]},"Conditions_of_Use":{"Digital_Object":["Born_digital","Digitised","Surrogate"],"Yes":[0.611,0.745,0.7896],"No":[0.389,0.255,0.2104]},"Renderability":{"Tech_Metadata":["Sufficient","Insufficient","Sufficient","Insufficient","Sufficient","Insufficient","Sufficient","Insufficient"],"Tools_to_Render":["Yes","Yes","No","No","Yes","Yes","No","No"],"Bit_Preservation":["Yes","Yes","Yes","Yes","No","No","No","No"],"Yes":[1,0.5993,0,0,0,0,0,0],"No":[0,0.4007,1,1,1,1,1,1]},"Bit_Preservation":{"Integrity":["Yes","No","Yes","No","Yes","No","Yes","No"],"Obsolescence":["Yes","Yes","No","No","Yes","Yes","No","No"],"Storage_Life":["Yes","Yes","Yes","Yes","No","No","No","No"],"Yes":[0,0,1,0.7158,0,0,0,0],"No":[1,1,0,0.2842,1,1,1,1]},"Identity":{"Info_Management":["Sufficient","Insufficient","Sufficient","Insufficient"],"Content_Metadata":["Yes","Yes","No","No"],"Yes":[1,0,0.5348,0],"No":[0,1,0.4652,1]},"Physical_Disaster":{"Yes":0.0005,"No":0.9995},"Storage_Life":{"Op_Environment":["Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No"],"Storage_Medium":["A","A","B","B","C","C","A","A","B","B","C","C","A","A","B","B","C","C","A","A","B","B","C","C"],"Rep_and_Refresh":["Good","Good","Good","Good","Good","Good","Poor","Poor","Poor","Poor","Poor","Poor","Good","Good","Good","Good","Good","Good","Poor","Poor","Poor","Poor","Poor","Poor"],"Physical_Disaster":["Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No","No","No","No","No","No","No","No","No","No","No","No"],"Yes":[1,0.5393,1,0.8747,1,1,0.7588,0.3212,0.945,0.646,1,1,1,1,1,1,1,1,0.7588,0.7588,0.945,0.945,1,1],"No":[0,0.4607,0,0.1253,0,0,0.2412,0.6788,0.055,0.354,0,0,0,0,0,0,0,0,0.2412,0.2412,0.055,0.055,0,0]},"Technical_Skills":{"Good":0,"Poor":1}}}],
            headers: {
                'Content-type': 'application/json; charset=UTF-8',
            },
        }).then((response) => {
            expect(response.status).to.eq(200);
            expect(response.body).to.include({"status": true});
        });
    });

    it('downloading a csv of the report data should succeed', () => {
        cy.request({
            method: 'POST',
            url: '/api/report/csv',
            body: [{"model_name":"my_model","scenario":"Base Model","notes":"","is_advanced":false,"intellectual_control":0,"renderability":45,"response":{"Digital_Object":[0,0,100],"Storage_Medium":[0,0,100],"Rep_and_Refresh":{"1":0,"2":0},"Op_Environment":{"1":0,"2":"Not Applicable - we have copies offsite"},"Physical_Disaster":"Very Low","Checksum":[0,0,100],"System_Security":{"1":"ISO 27001","2":"None, or only minor issues outstanding","3":"Level 4","4":"Yes"},"Info_Management":{"1":"Not achieved","2":"Not achieved","3":["Minimal awareness","Minimal awareness"]},"Technical_Skills":["None","None","None","None","None","None","None","None","None","None"]},"advanced":{"Op_Environment":{"Yes":0,"No":1},"Integrity":{"System_Security":["Good","Poor","Good","Poor","Good","Poor","Good","Poor","Good","Poor","Good","Poor"],"Info_Management":["Sufficient","Sufficient","Insufficient","Insufficient","Sufficient","Sufficient","Insufficient","Insufficient","Sufficient","Sufficient","Insufficient","Insufficient"],"Checksum":["Yes","Yes","Yes","Yes","Self_Generated","Self_Generated","Self_Generated","Self_Generated","No","No","No","No"],"Yes":[1,0.5326,0,0,0.9099,0.4424,0,0,0,0,0,0],"No":[0,0.4674,1,1,0.0901,0.5576,1,1,1,1,1,1]},"System_Security":{"Good":1,"Poor":0},"Info_Management":{"Sufficient":0,"Insufficient":1},"Storage_Medium":{"A":0,"B":0,"C":1},"Rep_and_Refresh":{"Good":0,"Poor":1},"Digital_Object":{"Born_digital":0,"Digitised":0,"Surrogate":1},"Content_Metadata":{"Digital_Object":["Born_digital","Digitised","Surrogate"],"Yes":[0.4539,0.7492,0.7261],"No":[0.5461,0.2508,0.2739]},"Tech_Metadata":{"Technical_Skills":["Good","Poor"],"Sufficient":[0.7729,0.427],"Insufficient":[0.2271,0.573]},"File_Format":{"Digital_Object":["Born_digital","Digitised","Surrogate"],"Yes":[0.8589,0.9999,0.9999],"No":[0.1411,0.0001,0.0001]},"Checksum":{"Yes":0,"Self_Generated":0,"No":1},"Obsolescence":{"Storage_Medium":["A","B","C","A","B","C"],"Technical_Skills":["Good","Good","Good","Poor","Poor","Poor"],"Yes":[0.2985,0.1405,0.001,0.6422,0.419,0.001],"No":[0.7015,0.8595,0.999,0.3578,0.581,0.999]},"Tools_to_Render":{"File_Format":["Yes","No","Yes","No"],"Technical_Skills":["Good","Good","Poor","Poor"],"Yes":[0.8111,0.4343,0.8111,0],"No":[0.1889,0.5657,0.1889,1]},"Intellectual_Control":{"Conditions_of_Use":["Yes","No","Yes","No"],"Identity":["Yes","Yes","No","No"],"Yes":[1,0,0,0],"No":[0,1,1,1]},"Conditions_of_Use":{"Digital_Object":["Born_digital","Digitised","Surrogate"],"Yes":[0.611,0.745,0.7896],"No":[0.389,0.255,0.2104]},"Renderability":{"Tech_Metadata":["Sufficient","Insufficient","Sufficient","Insufficient","Sufficient","Insufficient","Sufficient","Insufficient"],"Tools_to_Render":["Yes","Yes","No","No","Yes","Yes","No","No"],"Bit_Preservation":["Yes","Yes","Yes","Yes","No","No","No","No"],"Yes":[1,0.5993,0,0,0,0,0,0],"No":[0,0.4007,1,1,1,1,1,1]},"Bit_Preservation":{"Integrity":["Yes","No","Yes","No","Yes","No","Yes","No"],"Obsolescence":["Yes","Yes","No","No","Yes","Yes","No","No"],"Storage_Life":["Yes","Yes","Yes","Yes","No","No","No","No"],"Yes":[0,0,1,0.7158,0,0,0,0],"No":[1,1,0,0.2842,1,1,1,1]},"Identity":{"Info_Management":["Sufficient","Insufficient","Sufficient","Insufficient"],"Content_Metadata":["Yes","Yes","No","No"],"Yes":[1,0,0.5348,0],"No":[0,1,0.4652,1]},"Physical_Disaster":{"Yes":0.0005,"No":0.9995},"Storage_Life":{"Op_Environment":["Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No"],"Storage_Medium":["A","A","B","B","C","C","A","A","B","B","C","C","A","A","B","B","C","C","A","A","B","B","C","C"],"Rep_and_Refresh":["Good","Good","Good","Good","Good","Good","Poor","Poor","Poor","Poor","Poor","Poor","Good","Good","Good","Good","Good","Good","Poor","Poor","Poor","Poor","Poor","Poor"],"Physical_Disaster":["Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No","No","No","No","No","No","No","No","No","No","No","No"],"Yes":[1,0.5393,1,0.8747,1,1,0.7588,0.3212,0.945,0.646,1,1,1,1,1,1,1,1,0.7588,0.7588,0.945,0.945,1,1],"No":[0,0.4607,0,0.1253,0,0,0.2412,0.6788,0.055,0.354,0,0,0,0,0,0,0,0,0.2412,0.2412,0.055,0.055,0,0]},"Technical_Skills":{"Good":0,"Poor":1}}}],
            headers: {
                'Content-type': 'application/json; charset=UTF-8',
            },
        }).then((response) => {
            expect(response.status).to.eq(200);
            expect(response.body).to.include('name,scenario,notes,topic,question,part,response,intellectual_control,renderability');
            expect(response.body).to.include('my_model,Base Model,,Digital Object,What proportion of your digital archive are the following?,Born Digital (Records were created in a digital format),0,0,45')
            expect(response.body.trim().split('\n').length).to.eq(52);
        });
    });
});
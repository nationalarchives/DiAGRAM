describe('DiAGRAM API spec', () => {
    it('should get the model score when creating a new model', () => {
        cy.fixture('default-model.json').then((defaultModelPayload) => {
            cy.request({
                method: 'POST',
                url: '/api/model/score',
                body: defaultModelPayload,
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
    });

    it('should succeed in uploading an invalid model and report it to be invalid', () => {
        cy.fixture('invalid-model.json').then((invalidModelPayload) => {
            cy.request({
                method: 'POST',
                url: '/api/validation/validate_json',
                body: invalidModelPayload,
                headers: {
                    'Content-type': 'application/json; charset=UTF-8',
                },
            }).then((response) => {
                expect(response.status).to.eq(200);
                expect(response.body).to.include({"status": false});
            });
        });
    });

    it('should succeed in uploading a valid model and report it to be valid', () => {
        cy.fixture('valid-model.json').then((validModelPayload) => {
            cy.request({
                method: 'POST',
                url: '/api/validation/validate_json',
                body: validModelPayload,
                headers: {
                    'Content-type': 'application/json; charset=UTF-8',
                },
            }).then((response) => {
                expect(response.status).to.eq(200);
                expect(response.body).to.include({"status": true});
            });
        });
    });

    it('should produce a csv report using model and scenario data', () => {
        cy.fixture('report-data.json').then((reportDataPayload) => {
            cy.request({
                method: 'POST',
                url: '/api/report/csv',
                body: reportDataPayload,
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
});
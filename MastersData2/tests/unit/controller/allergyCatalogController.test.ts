import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { MongoServerError } from 'mongodb';
import { Container } from 'typedi';
import { Result } from '../../../src/core/logic/Result';
import IAllergyCatalogService from "../../../src/services/IServices/IAllergyCatalogService";
import AllergyCatalogController from "../../../src/controllers/allergyCatalogController";
import IAllergyCathalogItemDTO from '../../../src/dto/IAllergyCatalogItemDTO';
/*
describe('AllergyCatalogController', function() {
    let allergyServiceInstance: IAllergyCatalogService;
    let ctrl: AllergyCatalogController;
    let res: Partial<Response>;
    let res1: Partial<Response>;
    let req: Partial<Request>;
    let next: Partial<NextFunction>;

    beforeEach(function() {
        // Setup mock request and response
        req = {};
        res = {
            json: sinon.stub().returnsThis(),
            status: sinon.stub().returnsThis(),
            send: sinon.stub().returnsThis(),
        };
        res1 = {
            json: sinon.stub().returnsThis(),
            status: sinon.stub().returnsThis(),
            send: sinon.stub().returnsThis(),
        };
        next = sinon.spy();

        // Mock the allergy service instance directly
        allergyServiceInstance = {
            createAllergyCatalogItem: sinon.stub()
        } as unknown as IAllergyCatalogService;

        // Set the allergy service in the container manually
        Container.set('IAllergyCatalogService', allergyServiceInstance);

        // Create the controller instance
        ctrl = new AllergyCatalogController(allergyServiceInstance);
    });

    it('createAllergyCatalogItem: should return json with id and name value', async function () {
        const body = {
            code: "20241812",
            designation: "aaaa",
            description: "TesteABC123"
        };
        req.body = body;

        // Mock service method
        (allergyServiceInstance.createAllergyCatalogItem as sinon.SinonStub).resolves(Result.ok<IAllergyCathalogItemDTO>({
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123",
            designation: "des"
        }));

        // Call the controller method
        await ctrl.createAllergyCatalogItem(<Request>req, <Response>res, <NextFunction>next);

        // Check if response json was called with correct values
        sinon.assert.calledOnce(res.json as sinon.SinonSpy);
        sinon.assert.calledWith(res.json as sinon.SinonSpy, sinon.match({ designation: "des", id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f" }));
        sinon.assert.calledWith(res.status as sinon.SinonStub, 201); // Check if status is 201 (Created)
    });

    /*
    it('createAllergyCatalogItem: should return 400 on failure', async function () {
        const body = { name: 'invalidAllergy' };
        req.body = body;

        // Mock service method to return failure
        (allergyServiceInstance.createAllergyCatalogItem as sinon.SinonStub).rejects(Result.fail<IAllergyCathalogItemDTO>("Failed"));

        // Call the controller method
        await ctrl.createAllergyCatalogItem(<Request>req, <Response>res, <NextFunction>next);

        // Check if status 400 was returned
        sinon.assert.calledWith(res.status as sinon.SinonStub, 400);
    });

    it('createAllergyCatalogItem: should return 402 on duplicate key error', async function () {
        const body = {
            code: "20241812",
            designation: "aaaa",
            description: "TesteABC123"
        };
        req.body = body;

        // Simulate duplicate key error
        const error = new Error() as MongoServerError;
        error.code = 11000;
        (allergyServiceInstance.createAllergyCatalogItem as sinon.SinonStub).rejects(error);

        // Call the controller method
        
        await ctrl.createAllergyCatalogItem(<Request>req, <Response>res, <NextFunction>next);
        await ctrl.createAllergyCatalogItem(<Request>req, <Response>res, <NextFunction>next);

        // Check if status 409 was returned for duplicate key error

        sinon.assert.calledWith(res.status as sinon.SinonStub, 402);
        

    });
    
});*/
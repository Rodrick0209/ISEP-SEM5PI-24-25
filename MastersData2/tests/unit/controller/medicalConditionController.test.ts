import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import MedicalConditionController from '../../../src/controllers/medicalConditionController';
import IMedicalConditionService from '../../../src/services/IServices/IMedicalConditionService';
import IMedicalConditionCatalogDTO from '../../../src/dto/IMedicalConditionCatalogDTO';
import { Result } from "../../../src/core/logic/Result";
/*
describe('MedicalConditionController', () => {
    let medicalConditionServiceInstance: IMedicalConditionService;
    let ctrl: MedicalConditionController;
    let res: Partial<Response>;
    let req: Partial<Request>;
    let next: Partial<NextFunction>;

    beforeEach(() => {
        // Mock request, response, and next
        req = {};
        res = {
            json: sinon.spy(),
            status: sinon.stub().returnsThis(),
        };
        next = sinon.spy();

        // Mock the service instance
        medicalConditionServiceInstance = {
            getMedicalCondition: sinon.stub(),
            updateMedicalCondition: sinon.stub(),
            createMedicalCondition: sinon.stub(),
            listMedicalConditions: sinon.stub()
        } as unknown as IMedicalConditionService;

        // Manually set the service instance in the container
        Container.set('IMedicalConditionService', medicalConditionServiceInstance);

        // Create the controller
        ctrl = new MedicalConditionController(medicalConditionServiceInstance);
    });

    it('getMedicalCondition: should return medical condition when found', async () => {
        req.params = { code: 'A1234' };

        // Mock the service method
        (medicalConditionServiceInstance.getMedicalCondition as sinon.SinonStub).resolves(Result.ok<IMedicalConditionCatalogDTO>({
            id: '123e4567-e89b-12d3-a456-426614174001',
            code: 'A1234',
            designation: 'Condition A',
            description: ""
        }));

        // Call the controller method
        await ctrl.getMedicalCondition(<Request>req, <Response>res, <NextFunction>next);

        // Verify that the correct status code was set
        //sinon.assert.calledWith(res.status as sinon.SinonStub, 200);

        // Verify that the correct response body was sent
        sinon.assert.calledWith(res.json as sinon.SinonSpy, sinon.match({
            id: '123e4567-e89b-12d3-a456-426614174001',
            code: 'A1234',
            designation: 'Condition A'
        }));
    });

    it('getMedicalCondition: should return 404 when medical condition is not found', async () => {
        req.params = { code: 'A123' };

        // Mock the service method
        (medicalConditionServiceInstance.getMedicalCondition as sinon.SinonStub).resolves(Result.fail<IMedicalConditionCatalogDTO>('Not found'));

        // Call the controller method
        await ctrl.getMedicalCondition(<Request>req, <Response>res, <NextFunction>next);

        // Verify response
        sinon.assert.calledOnce(res.status as sinon.SinonStub);
        sinon.assert.calledWith(res.status as sinon.SinonStub, 404);
    });

    it('updateMedicalCondition: should return updated medical condition', async () => {
        const body = { code: 'A123', name: 'Updated Condition A' };
        req.params = { code: 'A123' };
        req.body = body;

        // Mock the service method
        (medicalConditionServiceInstance.updateMedicalCondition as sinon.SinonStub).resolves(Result.ok<IMedicalConditionCatalogDTO>({
            code: 'A123',
            name: 'Updated Condition A'
        }));

        // Call the controller method
        await ctrl.updateMedicalCondition(<Request>req, <Response>res, <NextFunction>next);

        // Verify response
        sinon.assert.calledOnce(res.json as sinon.SinonSpy);
        sinon.assert.calledWith(res.json as sinon.SinonSpy, sinon.match({ code: 'A123', name: 'Updated Condition A' }));
        //sinon.assert.calledWith(res.status as sinon.SinonStub, 200);
    });

    it('updateMedicalCondition: should return 400 on failure', async () => {
        const body = { name: 'Invalid Condition' };
        req.params = { code: 'A123' };
        req.body = body;

        // Mock the service method to return failure
        (medicalConditionServiceInstance.updateMedicalCondition as sinon.SinonStub).resolves(Result.fail<IMedicalConditionCatalogDTO>('Failed'));

        // Call the controller method
        await ctrl.updateMedicalCondition(<Request>req, <Response>res, <NextFunction>next);

        // Verify response
        sinon.assert.calledOnce(res.status as sinon.SinonStub);
        sinon.assert.calledWith(res.status as sinon.SinonStub, 400);
    });

    it('createMedicalCondition: should return created medical condition', async () => {
        const body = { name: 'New Condition' };
        req.body = body;

        // Mock the service method
        (medicalConditionServiceInstance.createMedicalCondition as sinon.SinonStub).resolves(Result.ok<IMedicalConditionCatalogDTO>({
            code: 'B123',
            name: 'New Condition'
        }));

        // Call the controller method
        await ctrl.createMedicalCondition(<Request>req, <Response>res, <NextFunction>next);

        // Verify response
        sinon.assert.calledOnce(res.json as sinon.SinonSpy);
        sinon.assert.calledWith(res.json as sinon.SinonSpy, sinon.match({ code: 'B123', name: 'New Condition' }));
        //sinon.assert.calledWith(res.status as sinon.SinonStub, 201);
    });

    it('createMedicalCondition: should return 400 on failure', async () => {
        req.body = { name: 'Invalid Condition' };

        // Mock the service method to return failure
        (medicalConditionServiceInstance.createMedicalCondition as sinon.SinonStub).resolves(Result.fail<IMedicalConditionCatalogDTO>('Failed'));

        // Call the controller method
        await ctrl.createMedicalCondition(<Request>req, <Response>res, <NextFunction>next);

        // Verify response
        sinon.assert.calledOnce(res.status as sinon.SinonStub);
        sinon.assert.calledWith(res.status as sinon.SinonStub, 400);
    });

    it('getAllMedicalConditions: should return all medical conditions', async () => {
        const body = [{ code: 'A123', name: 'Condition A' }];
        req.body = body;

        // Mock the service method
        (medicalConditionServiceInstance.listMedicalConditions as sinon.SinonStub).resolves(Result.ok<IMedicalConditionCatalogDTO[]>([
            { code: 'A123', name: 'Condition A' }
        ]));

        // Call the controller method
        await ctrl.getAllMedicalConditions(<Request>req, <Response>res, <NextFunction>next);

        // Verify response
        sinon.assert.calledOnce(res.json as sinon.SinonSpy);
        sinon.assert.calledWith(res.json as sinon.SinonSpy, sinon.match([{ code: 'A123', name: 'Condition A' }]));
        sinon.assert.calledWith(res.status as sinon.SinonStub, 200);
    });

    it('getAllMedicalConditions: should return 404 if no conditions found', async () => {
        // Mock the service method
        (medicalConditionServiceInstance.listMedicalConditions as sinon.SinonStub).resolves(Result.fail<IMedicalConditionCatalogDTO[]>('No conditions found'));

        // Call the controller method
        await ctrl.getAllMedicalConditions(<Request>req, <Response>res, <NextFunction>next);

        // Verify response
        sinon.assert.calledOnce(res.status as sinon.SinonStub);
        sinon.assert.calledWith(res.status as sinon.SinonStub, 404);
    });
});*/
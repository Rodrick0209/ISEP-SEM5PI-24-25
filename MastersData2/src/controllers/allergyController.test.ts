import * as sinon from 'sinon';

import { Response, Request, NextFunction } from 'express';

import { Container } from 'typedi';
import config from "../../config";

import { Result } from '../core/logic/Result';


import IAllergyCatalogService from "../services/IServices/IAllergyCatalogService";
import AllergyCatalogController from "./allergyCatalogController";
import IAllergyCathalogItemDTO from '../dto/IAllergyCatalogItemDTO';


describe('allergy controller', function(){
    beforeEach(function() {
    });

    it('createAllergy: returns json with id+name value', async function () {
        let body = { "name":'allergy12' };
        let req: Partial<Request> = {};
        req.body = body;


        let res: Partial<Response> = {
            json: sinon.spy()
        };

        let next: Partial<NextFunction> = () => {};

        let allergyServiceClass = require(config.services.allergy.path).default;
        let allergyServiceInstance = Container.get(allergyServiceClass)
        Container.set(config.services.allergy.name, allergyServiceInstance);
        
        allergyServiceInstance = Container.get(config.services.allergy.name); 
        sinon.stub(allergyServiceInstance, "createAllergy").returns( Result.ok<IAllergyCathalogItemDTO>( {"id":"123","name": req.body.name} ));

        const ctrl = new AllergyCatalogController(allergyServiceInstance as IAllergyCatalogService);

        await ctrl.createAllergyCatalogItem(<Request>req, <Response>res, <NextFunction>next);

        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match({ "name": req.body.name}));
    });

});
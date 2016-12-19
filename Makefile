build:
	stack docker pull
	stack build
	docker build -t build-server . -f Dockerfile.build
	docker tag my-repo/build-server:latest
	docker push my-repo/build-server:latest
	docker build -t hook-server . -f Dockerfile.hook
	docker tag my-repo/hook-server:latest
	docker push my-repo/hook-server:latest

# side load these so we can avoid git trying to add them later
# you *really* want to manually verify these fingerprints
# this command is here as an example of a step that should be taken
scan-keys:
	ssh-keyscan bitbucket.org >> /tmp/known_hosts

# note that this example uses AWS, so we side load some relevant credentials here as well
build-secrets: scan-keys
	kubectl --namespace build delete secret build-ssh-keys
	kubectl --namespace build create secret generic build-ssh-keys \
		--from-file=id_rsa=.ssh/id_rsa \
		--from-file=id_rsa.pub=.ssh/id_rsa.pub \
		--from-file=known_hosts=/tmp/known_hosts
	kubectl --namespace build delete secret aws-keys
	kubectl --namespace build create secret generic aws-keys \
		--from-file=credentials=.aws/credentials

build-create:
	kubectl --namespace=build create -f kubernetes/
	kubectl --namespace build expose rc nginx-ingress-controller --type=LoadBalancer --port=80 --target-port=80
